#!/usr/bin/env bash

set -eu

log() { printf "$@" >&2; }

lastRelease=${1:-$(git tag --list --sort -version:refname | head -n1)}

log 'Last release is `%s`.\n' "$lastRelease"

get-changelog-entries() {
	# We format manually via `sed`, since trailers contain trailing newlines
	entries=$(git log HEAD..."$lastRelease" \
		-z --format="%h%x09%(trailers:valueonly,key=$1)" \
		| sed -zne 's/^\([0-9a-f]\+\)\t\(.\+\)\n$/- \2 (\1)/p' \
		| tr '\0' '\n')

	if [[ -z "$entries" ]] then return; fi

	printf '\n### %s\n\n' "$1"
	printf '%s\n' "$entries"
}

printf '## Unreleased\n'
get-changelog-entries Breaking
get-changelog-entries Added
get-changelog-entries Changed
get-changelog-entries Fixed
