// This file is part of Substrate.

// Copyright (C) 2020 Parity Technologies (UK) Ltd.
// SPDX-License-Identifier: Apache-2.0

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// 	http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! TODO:

use crate::{IdentifierT, Voter, Edge, ExtendedBalance};
use sp_arithmetic::traits::Zero;
use sp_std::prelude::*;

pub fn balance<AccountId: IdentifierT>(
	voters: &mut Vec<Voter<AccountId>>,
	iterations: usize,
	tolerance: ExtendedBalance,
) -> usize {
	if iterations == 0 { return 0; }

	let mut iter = 0 ;
	loop {
		let mut max_diff = 0;
		for voter in voters.iter_mut() {
			let diff = balance_voter(voter, tolerance);
			if diff > max_diff { max_diff = diff; }
		}

		iter += 1;
		if max_diff < tolerance || iter >= iterations {
			break iter;
		}
	}
}

pub fn balance_voter<AccountId: IdentifierT>(
	voter: &mut Voter<AccountId>,
	tolerance: ExtendedBalance,
) -> ExtendedBalance {
	// this will be a read-only snapshot. We will re-order them, but this will not matter. All
	// updates will be applied to `voter`.
	let mut elected_edges = voter.edges
		.iter()
		.filter(|e| e.candidate.borrow().elected)
		.cloned()
		.collect::<Vec<Edge<_>>>();

	// Either empty, or a self vote. Not much to do in either case.
	if elected_edges.len() <= 1 {
		return Zero::zero()
	}

	let stake_used = elected_edges.iter().fold(0, |a, e| a.saturating_add(e.weight));
	let backed_stakes = elected_edges
		.iter()
		.map(|e| e.candidate.borrow().backed_stake)
		.collect::<Vec<_>>();
	let backing_backed_stake = elected_edges
		.iter()
		.filter_map(|e| if e.weight > 0 { Some(e.candidate.borrow().backed_stake) } else { None })
		.collect::<Vec<_>>();

	let difference = if backing_backed_stake.len() > 0 {
		let max_stake = backing_backed_stake
			.iter()
			.max()
			.expect("vector with positive length will have a max; qed");
		let min_stake = backed_stakes
			.iter()
			.min()
			.expect("iterator with positive length will have a min; qed");
		let mut difference = max_stake.saturating_sub(*min_stake);
		difference = difference.saturating_add(voter.budget.saturating_sub(stake_used));
		if difference < tolerance {
			return difference;
		}
		difference
	} else {
		voter.budget
	};

	// remove all backings.
	for edge in voter.edges.iter_mut() {
		// TODO: make this one liner.
		let mut candidate = edge.candidate.borrow_mut();
		candidate.backed_stake = candidate.backed_stake.saturating_sub(edge.weight);
		edge.weight = 0;
	}

	elected_edges.sort_unstable_by_key(|e| e.candidate.borrow().backed_stake);

	let mut cumulative_backed_stake = Zero::zero();
	let mut last_index = elected_edges.len() - 1;

	for (index, edge) in voter.edges.iter_mut().enumerate() {
		let index = index as ExtendedBalance;
		let backed_stake = edge.candidate.borrow().backed_stake;
		let temp = backed_stake.saturating_mul(index);
		if temp.saturating_sub(cumulative_backed_stake) > voter.budget {
			// defensive only. length of elected_edges is checked to be above 1.
			last_index = index.saturating_sub(1) as usize;
			break
		}
		cumulative_backed_stake = cumulative_backed_stake.saturating_add(backed_stake);
	}

	let last_stake = elected_edges.get(last_index).expect(
		"length of elected_edges is greater than or equal 2; last_index index is at \
		the minimum elected_edges.len() - 1; index is within range; qed"
	).candidate.borrow().backed_stake;
	let ways_to_split = last_index + 1;
	let excess = voter.budget
		.saturating_add(cumulative_backed_stake)
		.saturating_sub(last_stake.saturating_mul(ways_to_split as ExtendedBalance));

	// Do the final update.
	for edge in voter.edges.iter_mut().take(ways_to_split) {
		let mut candidate = edge.candidate.borrow_mut();
		edge.weight = (excess / ways_to_split as ExtendedBalance)
			.saturating_add(last_stake)
			.saturating_sub(candidate.backed_stake);
		candidate.backed_stake = candidate.backed_stake.saturating_add(edge.weight);
	}

	difference
}
