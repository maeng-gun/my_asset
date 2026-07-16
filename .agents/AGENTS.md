# Database Access Guidelines

- **Postgres MCP Interface Only:** When interacting with or querying the database, exclusively use the host-based Postgres MCP interface declared in `mcp_config.json`. Do not use `Rscript`, shell commands, or custom R scripts to access the database.
