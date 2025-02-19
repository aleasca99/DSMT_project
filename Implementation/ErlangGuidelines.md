# Erlang Development Guidelines

This document provides guidelines for developing Erlang components within the Whenly project. It covers prerequisites, module interactions, configuration, best practices, and common commands used during development and deployment.

---

## Prerequisites

### Installing Erlang

On Debian/Ubuntu, install Erlang with the following commands:

```bash
sudo apt-get update
sudo apt-get install erlang
```

---

## Module Interactions

### Importing Functions from Other Modules

In Erlang, functions are invoked using the syntax `Module:Function/Arity`. Although there is no traditional "import" mechanism (as in languages like Python), you can use the `-import` directive to avoid repeatedly specifying the module name. Use this feature sparingly, as explicit module qualification often improves code readability.

---

## Configuration and Environment Variables

- **Custom Configuration Files:**  
  You can use custom configuration files (e.g., JSON, YAML) and parse them at runtime. This externalizes configurable parameters such as node names, timeouts, or other environment-specific values.
  
- **Environment Variables:**  
  Environment variables can also be used to override configuration values at startup.

---

## Best Practices

- **Avoid Hardcoding:**  
  Always externalize values that may change (node names, timeouts, etc.) into configuration files or environment variables.
  
- **Modularize Your Code:**  
  Consolidate reusable functions into utility modules to promote code reuse and reduce duplication.
  
- **Document Configuration:**  
  Clearly document all required configuration keys and their expected values in your project documentation.

Following these practices will help you develop modular, configurable, and maintainable Erlang code.

---

## What is Rebar3?

[Rebar3](https://www.rebar3.org/) is the official build tool for Erlang projects. It simplifies development by managing dependencies, compiling code, running tests, creating releases, and more. Its streamlined workflow is especially beneficial when integrating external libraries like `jsx` for JSON processing.

### Installing Rebar3 on Debian/Ubuntu

To install Rebar3 from source (recommended for cross-platform compatibility), follow these steps:

```bash
git clone https://github.com/erlang/rebar3.git
cd rebar3
./bootstrap
./rebar3 local install
```

This process compiles a rebar3 escript that can be installed globally.

---

## Basic Syntax and Project Structure

### Creating a New Application

Create a new Erlang application using:

```bash
rebar3 new app <project_name>
```

This command generates a project structure similar to:

```
my_project/
├── rebar.config
├── src/
│   ├── my_project.app.src
│   ├── my_project.erl
│   ├── my_project_sup.erl
└── test/
    ├── my_project_SUITE.erl
```

- **src/**: Contains application modules and the supervision tree.
- **test/**: Contains test cases.
- **rebar.config**: The primary configuration file for your project.

---

## Example: Creating an Erlang App with Dependencies

1. **Create the Application:**

   ```bash
   rebar3 new app my_project
   ```

2. **Navigate to the Project Directory:**

   ```bash
   cd my_project
   ```

3. **Edit `rebar.config` to Add Dependencies:**

   For example, to add `jsx` for JSON parsing:

   ```erlang
   {deps, [ jsx ]}.
   ```

4. **Fetch Dependencies:**

   ```bash
   rebar3 get-deps
   ```

5. **Compile the Application:**

   ```bash
   rebar3 compile
   ```

---

## Running Your Application

### 1. Compiling

Always ensure your code is compiled before running:

```bash
rebar3 compile
```

### 2. Interactive Shell

For development and debugging, start an interactive shell with:

```bash
rebar3 shell
```

*Description:* Launches an Erlang shell (`erl`) with your application and its dependencies loaded.

### 3. Creating a Release (For Deployment)

1. **Define a Release in `rebar.config`:**

   ```erlang
   {relx, [{release, {my_project, "0.1.0"}, [my_project]}]}.
   ```

2. **Build the Release:**

   ```bash
   rebar3 release
   ```

   *Description:* The release package will be generated in `_build/default/rel/my_project`.

### 4. Running in Detached Mode

For production, run your release as a daemon:

```bash
_build/default/rel/my_project/bin/my_project daemon
```

To stop the application:

```bash
_build/default/rel/my_project/bin/my_project stop
```

---

## Deployment and Container Commands

### Erlang Shell Commands

- **Fully Qualified Name:**

  ```bash
  rebar3 shell --name event_serverX@MachineName --setcookie whenly
  ```

  *Description:* Launches an Erlang shell with a fully qualified node name. Replace `event_serverX` with your node's name and `MachineName` with the host's domain or IP.

- **Short Name:**

  ```bash
  rebar3 shell --sname event_serverX --setcookie whenly
  ```

  *Description:* Starts an Erlang shell using a short node name format. Replace `event_serverX` with your node's name.

### Used Containers

- **Event Server Nodes:**
  - `event_server1@10.2.1.9`
  - `event_server2@10.2.1.10`
  - `event_server3@10.2.1.21`

- **Erlang Backend:**
  - `erlang_backend@10.2.1.11`

### File Copying to Containers

To copy files or directories to a container, use:

```bash
scp -r folder root@IP:/root/
```

*Description:* This command copies the specified `folder` recursively to the container at the provided IP address. Replace `folder` with your directory and `IP` with the container's IP.

### Running a Release on a Container

After executing the `rebar3 release` command, the release package is located in:

```
_build/default/rel/project_name
```

Copy this folder to the container using `scp` as shown above.

### Keeping the Application Running Using Screen

To keep your application running on the container, use the `screen` utility combined with the release console:

```bash
./project_name/bin/project_name console
```

Run this command inside a screen session.

#### Linux Screen Commands

- **Creating a Screen Session:**

  ```bash
  screen -S session_name
  ```

  *Description:* Create a new screen session with the name `session_name`.

- **Detaching a Screen Session:**

  Press `Ctrl-A` then `d`

  *Description:* Detach from the current screen session, leaving it running in the background.

- **Listing Screen Sessions:**

  ```bash
  screen -ls
  ```

  *Description:* List all active screen sessions.

- **Removing a Screen Session:**

  ```bash
  screen -X -S session_name quit
  ```

  *Description:* Terminate the screen session named `session_name`.

---

## Which Method Should You Use?

- **For Development/Testing:**  
  Use `rebar3 shell` for an interactive environment.
  
- **For Deployment:**  
  Use `rebar3 release` to create a self-contained release and run the binary as a standalone service, possibly within a screen session for continuous operation.

---

## References

- [Supervisor Behaviour](https://www.erlang.org/doc/system/sup_princ.html)
- [gen_server Behaviour](https://www.erlang.org/doc/apps/stdlib/gen_server#start_link/4)
- [application Behaviour](https://www.erlang.org/doc/apps/kernel/application.html)
- [mnesia](https://www.erlang.org/doc/apps/mnesia/mnesia.html)