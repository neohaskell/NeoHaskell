# Source playbooks

The why skill spawns one investigator per available evidence category, each reading the applicable source-specific playbooks below. Sources may be accessible through local files, a CLI, an API, or a connector. Keep one investigator responsible for each category, and record coverage per actual source. The provider playbooks remain examples; use only tools that are actually available.

| Category | Playbook | Source or example interface |
|---|---|---|
| Source control history | [`code-archaeology.md`](sources/code-archaeology.md) | git, `gh` |
| Issue / ticket tracker | [`task-tracker.md`](sources/task-tracker.md) | The project's task tracker through an available read interface |
| Issue / ticket tracker | [`linear.md`](sources/linear.md) | Linear (adapt for Jira, GitHub Issues, Plane, Shortcut) |
| Long-form documents | [`design-documents.md`](sources/design-documents.md) | The project's design documents through available local or remote sources |
| Long-form documents | [`notion.md`](sources/notion.md) | Notion (adapt for Confluence, Google Docs, Coda) |
| Real-time team chat | [`slack.md`](sources/slack.md) | Slack (adapt for Discord, Microsoft Teams, Mattermost) |
| Infrastructure observability | [`datadog.md`](sources/datadog.md) | Datadog (adapt for New Relic, Honeycomb, Grafana, Splunk) |
| Error / exception tracking | [`sentry.md`](sources/sentry.md) | Sentry (adapt for Rollbar, Bugsnag, Airbrake) |
| Product analytics warehouse | [`databricks.md`](sources/databricks.md) | Databricks SQL (adapt for Snowflake, BigQuery, ClickHouse, dbt) |

Cross-cutting:

- [`incident-postmortem.md`](sources/incident-postmortem.md). Add this if the target code looks defensive (null checks, retry, timeout, rate limit, feature flag, egress guard, OOM handler).
