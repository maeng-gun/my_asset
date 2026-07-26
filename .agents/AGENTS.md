# Database Access Guidelines

- **Postgres MCP Interface Only:** When interacting with or querying the database, exclusively use the host-based Postgres MCP interface declared in `mcp_config.json`. Do not use `Rscript`, shell commands, or custom R scripts to access the database.

# Application & Sandbox Synchronization

- **Sync app.R and sandbox.R:** When modifying module calls, UI layouts, server function arguments, or global configurations in `app.R`, you **MUST simultaneously reflect the exact same modifications in `sandbox.R`**. Because interactive development and feature verification are executed via `sandbox.R`, failing to synchronize both files causes discrepancies and test failures. Always examine and update both `app.R` and `sandbox.R` together.
