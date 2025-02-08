# Event Server Application

## Description

The Event Server Application is a distributed event scheduling service built using OTP and rebar3. It is responsible for managing event creation requests, receiving and processing scheduling constraints, and coordinating the distributed computation of final event solutions. The application utilizes Mnesia for persistent storage and OTP supervision to ensure fault tolerance and high availability. It works in conjunction with the Erlang Backend Application to provide complete event scheduling functionality.

## Testing

To test the application in interactive mode, run the following command from the project root:

```bash
rebar3 shell --sname event_server1 --setcookie whenly
```

This command starts an Erlang shell with the node name `event_server1` and sets the cookie to `whenly` for proper distributed communication.

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

   The release package will be generated in `_build/default/rel/event_server`.

### Configuring the Release

- **VM Arguments:**  
  Ensure you have a `vm.args` file in the `rel/` directory that includes your node name and cookie settings. For example:

  ```
  -name event_server1@10.2.1.10 -setcookie whenly
  ```

- **Configuration File:**
  The `config.json` file at the project root, should be copied manually into the release directory `_build/default/rel/event_server\`.

### Running the Release

To start your application in console mode, navigate to the release directory and run:

```bash
cd _build/default/rel/event_server
./bin/event_server console
```

## Additional Notes

- **Distributed Setup:**  
  Ensure that all nodes in your distributed system are started with the same cookie (`whenly`) and that node names are configured appropriately (using `-sname` for short names or `-name` for fully qualified names).

- **Monitoring & Logging:**  
  The application leverages OTP’s built-in monitoring and SASL for logging and crash reporting. Review logs in the `log/` directory of your release for any runtime issues.

- **Mnesia Storage:**  
  The Event Server uses Mnesia for the persistent storage of event deadlines and partial solutions.
