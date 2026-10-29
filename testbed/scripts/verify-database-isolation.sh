#!/usr/bin/env bash

set -euo pipefail

testbed_db="$1"
oauth_db="$2"

if [ "$testbed_db" = "$oauth_db" ]; then
	echo "Testbed applications used the same database: $testbed_db" >&2
	exit 1
fi

export PGPASSWORD="${DB_PASSWORD:-neohaskell}"
psql_args=(-h "${DB_HOST:-localhost}" -p "${DB_PORT:-5432}" -U "${DB_USER:-neohaskell}" -v ON_ERROR_STOP=1 -At -F '|')

testbed_events=$(psql "${psql_args[@]}" -d "$testbed_db" -c "
SELECT
  count(*) FILTER (WHERE EventData->>'tag' = 'ExternalEffectDelivered'),
  count(*) FILTER (WHERE EventData->>'tag' = 'ExternalEffectFailed'),
  count(*) FILTER (WHERE Entity = 'AccountConnectionEntity')
FROM Events")
oauth_events=$(psql "${psql_args[@]}" -d "$oauth_db" -c "
SELECT
  count(*) FILTER (WHERE EventData->>'tag' = 'AccountTokenPresenceObserved' AND EventData->>'tokenStored' = 'true'),
  count(*) FILTER (WHERE EventData->>'tag' = 'AccountTokenPresenceObserved' AND EventData->>'tokenStored' = 'false'),
  count(*) FILTER (WHERE Entity = 'ExternalEffectEntity')
FROM Events")

IFS='|' read -r delivered failed account_leak <<< "$testbed_events"
IFS='|' read -r stored removed effect_leak <<< "$oauth_events"

if (( delivered < 1 || failed < 1 || stored < 1 || removed < 1 || account_leak != 0 || effect_leak != 0 )); then
	echo "Event-store isolation failed. Testbed delivered=$delivered failed=$failed account=$account_leak. OAuth stored=$stored removed=$removed external=$effect_leak." >&2
	exit 1
fi

echo "Event-store isolation passed. Testbed=$testbed_db OAuth=$oauth_db."
