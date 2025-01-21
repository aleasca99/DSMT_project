## **WHENLY**
### **A Distributed Event Scheduling Application**

#### **Architecture Overview**

1. **WebApp Node**

   - A web-based frontend implemented with **JSP** hosted on an **Apache Tomcat** server.
   - Responsibilities:
     - Handles user interaction for event creation and constraint submission.
     - Authenticates users by interacting with the database node.
     - Distributes event-related requests in a **round-robin manner** to the **event server nodes** for load balancing.

2. **Database Node**

   - A **MySQL database** for centralized storage of:
     - **User credentials:** (username, password).
     - **Constraints:** User-submitted constraints and the corresponding **event server** node responsible for that event.
     - **Completed events:** Stores final solutions for past events, allowing for constraint and log cleanup.

3. **Event Server Nodes** (Minimum 2, scalable for load balancing)

   - Responsibilities:
     - **Distributed constraint management:** Each node stores constraints for the events it handles in a **Redis database**.
     - **Partial solution calculation:** Maintains the current intersection of constraints for the events it handles.
     - **Distributed coordination:** Executes the distributed algorithm to compute the final schedule when the **deadline** expires.

---

#### **Key Workflow**

1. **Event Creation (Handled by WebApp Node)**

   - The **administrator** creates an event by submitting:
     - Event details (e.g., description, participants).
     - Deadline for scheduling.
     - Their own constraints.
   - The WebApp Node:
     - Authenticates the user via the **database node**.
     - Assigns the event to one of the **event server nodes** using a **round-robin load balancing strategy**.
     - Sends the event and constraints to the chosen event server.

2. **Constraint Submission (Handled by Event Server Nodes)**

   - Users submit constraints via the WebApp.
   - The WebApp redirects the request to one of the **event server nodes** using a **round-robin load balancing strategy**.
   - The receiving **event server**:
     - Stores the new constraints locally in its **Redis database**.
     - Updates the **partial solution** by intersecting the new constraints with the current solution (if any) for the event.

3. **Distributed Solution Computation**

   - When the **deadline** expires:
     - The **event server** that accepted the administrator's first request (and therefore stored the event deadline) initiates the distributed algorithm. This occurs only if its local queue is empty; otherwise, it waits until the queue clears or a timeout occurs.
     - It communicates with all other event servers to exchange the partial solutions they have stored for the event. Together, they compute a final solution if one exists; otherwise, they agree there is no solution.

4. **Failure Recovery**

   - If an **event server** goes down:
     - The **WebApp node** retrieves constraints from the **MySQL database**, which maintains backup records of constraints and event-server mappings.
     - Constraints from the failed server are restored, and another **event server** takes over execution.

5. **Final Solution Storage**

   - After the schedule is computed:
     - The solution is stored in the **MySQL database**.
     - All temporary constraints are deleted from the Redis databases of the event servers.
     - Constraints related to the event are also removed from the MySQL database since they were only needed for recovery.

---

### **Algorithm for Distributed Event Scheduling**

#### **Algorithm Workflow**

1. **Event Creation (Initialization Phase)**

   - The **primary event server** (the one that receives the first request for an event) becomes the **leader** for that event.
   - The leader:
     - Records the event's deadline.
     - Waits for constraint submissions and stores them in **Redis**.
     - Continuously computes partial intersections of constraints.

2. **Constraint Submission and Partial Solution Update**

   - When a constraint is submitted, the server:
     - Validates the format and stores it in Redis.
     - Updates the **partial solution** by intersecting the current solution with the new constraint.
     - If the event ID is unknown (i.e., the event was created on a different server):
       - It stores the constraints locally but does not act as the leader.
       - Relies on the leader to execute the final computation.

3. **Final Solution Calculation (Execution Phase)**

   - When the **deadline** expires, the leader:
     - Ensures all request queues on all event servers are empty.
     - Sends a synchronization signal to all servers to freeze updates.
     - Collects constraints and partial solutions from other servers.
   - The leader computes the **final solution**:
     - Intersects all received constraints to find a valid time slot.
     - If no solution exists, it reports failure after the deadline.

4. **Result Distribution and Cleanup**

   - The final solution is broadcast to all nodes and stored in the MySQL database.
   - Redis databases clean up all temporary constraint data for the event.

---

### **Considerations**

1. **Event Server Failures**

   - All event constraints are backed up in the **MySQL database**, enabling recovery if a server crashes.
   - Use a **heartbeat mechanism** to monitor server health, and reassign events to healthy servers dynamically.

2. **Synchronization and Deadlines**

   - Only the **leader** node knows the event deadline and ensures no duplicate computations occur.
   - Synchronization between servers is achieved using a lightweight **message-passing protocol** (e.g., Erlang’s built-in messaging or gRPC).

3. **Concurrency Control**

   - Use a **queue-based processing model** for constraint submissions to prevent race conditions.
   - Redis’ atomic operations ensure consistency during constraint updates.

4. **Scalability**

   - Add **more event servers** as load increases.
   - The round-robin assignment ensures even distribution of load across servers.

---

### **Tools and Technologies**

1. **Frontend and WebApp**

   - **Java JSP** with **Apache Tomcat** for the user-facing component.
   - Handles user input and redirects requests to event servers.

2. **Backend Event Servers**

   - **Erlang** for distributed coordination and partial solution computation.
   - **Redis** for fast in-memory storage of constraints.
   - Lightweight **message-passing protocols** for synchronization (e.g., Erlang messaging).

3. **Database**

   - **MySQL** for persistent storage of user data, constraints, and completed events.

4. **Load Balancing**

   - Round-robin mechanism implemented at the WebApp node for distributing user requests to event servers.

5. **Recovery**

   - The **WebApp node** serves as the fail-safe by recovering data from the **MySQL database** if event servers fail.

---

### **Advantages**

- **Scalability:** Easily add more event servers for larger workloads.
- **Fault Tolerance:** Central database and recovery mechanisms handle server failures.
- **Efficiency:** Partial constraint updates avoid recalculating the solution from scratch.
- **Consistency:** Redis ensures atomic operations and consistency during updates.

