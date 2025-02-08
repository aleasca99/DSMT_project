# Erlang Backend Application

## Description

The Erlang Backend Application is a distributed backend service built using OTP and rebar3. It provides APIs for sending event creation and constraint requests to Erlang Event Server nodes, monitors these nodes for availability, and receives final solutions from the coordinator. The application is designed to interface with Java components via JInterface.

## Testing

To test the application in interactive mode, run the following command from the project root:

```bash
rebar3 shell --sname erlang_backend --setcookie whenly
```

This command starts an Erlang shell with the node name `erlang_backend` and sets the cookie to `whenly` for proper distributed communication.

## Deployment

### Building a Release

1. **Compile the Application:**

   ```bash
   rebar3 compile
   ```

2. **Create the Release:**

   ```bash
   rebar3 release
   ```

   The release package will be generated in `_build/default/rel/erlang_backend`.

### Configuring the Release

- **VM Arguments:**  
  Ensure you have a `vm.args` file in the `rel/` directory that includes your node name and cookie settings. For example:
  ```
  -name erlang_backend@10.2.1.9 -setcookie whenly
  ```

- **Configuration File:**  
  The `config.json` file at the project root, should be copied manually into the release directory `_build/default/rel/erlang_backend\`.

### Running the Release

To start your application in console mode, navigate to the release directory and run:

```bash
cd _build/default/rel/erlang_backend
./bin/erlang_backend console
```

## Additional Notes

- **Distributed Setup:**  
  Ensure that all nodes in your distributed system are started with the same cookie (`whenly`) and that node names are configured appropriately (using `-sname` for short names or `-name` for fully qualified names).

- **Monitoring & Logging:**  
  The application leverages OTP’s built-in monitoring and SASL for logging and crash reporting. Review logs in the `log/` directory of your release for any runtime issues.
