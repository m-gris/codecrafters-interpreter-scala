#!/bin/sh
# Run the official CodeCrafters tester locally against chosen stages.
#
#   ./oracle.sh                # fuzzy-pick stages with fzf (TAB to multi-select)
#   ./oracle.sh ry8            # one stage
#   ./oracle.sh ry8 ol4 oe8    # several, in order
#
# The tester binary is built from https://github.com/codecrafters-io/interpreter-tester
# (go build -o dist/main.out ./cmd/tester). Override its location with CODECRAFTERS_TESTER.

set -e

REPO="$(dirname "$(realpath "$0")")"
TESTER="${CODECRAFTERS_TESTER:-$REPO/../interpreter-tester/dist/main.out}"

# rank<TAB>slug<TAB>title — order and slugs from the tester's internal/tester_definition.go
STAGES='01	ry8	Scanning: Empty file
02	ol4	Scanning: Parentheses
03	oe8	Scanning: Braces
04	xc5	Scanning: Single-character tokens
05	ea6	Scanning: Lexical errors
06	mp7	Scanning: Equality operators
07	bu3	Scanning: Negation operators
08	et2	Scanning: Relational operators
09	ml2	Scanning: Comments
10	er2	Scanning: Whitespace
11	tz7	Scanning: Multi-line errors
12	ue7	Scanning: String literals
13	kj0	Scanning: Number literals
14	ey7	Scanning: Identifiers
15	pq5	Scanning: Reserved words'

if [ $# -eq 0 ]; then
  set -- $(printf '%s\n' "$STAGES" | fzf --multi --delimiter='\t' --prompt='stage> ' | cut -f2)
  [ $# -gt 0 ] || { echo "no stage selected" >&2; exit 2; }
fi

cases=""
for slug in "$@"; do
  title=$(printf '%s\n' "$STAGES" | awk -F'\t' -v s="$slug" '$2 == s { print $3 }')
  [ -n "$title" ] || { echo "unknown stage slug: $slug" >&2; exit 2; }
  [ -n "$cases" ] && cases="$cases,"
  cases="$cases{\"slug\":\"$slug\",\"tester_log_prefix\":\"$slug\",\"title\":\"$title\"}"
done

CODECRAFTERS_REPOSITORY_DIR="$REPO" \
CODECRAFTERS_SUBMISSION_DIR="$REPO" \
CODECRAFTERS_TEST_CASES_JSON="[$cases]" \
exec "$TESTER"
