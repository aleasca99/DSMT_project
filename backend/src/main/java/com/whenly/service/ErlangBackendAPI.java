package com.whenly.service;

import com.ericsson.otp.erlang.*;
import com.whenly.events.FinalSolutionEvent;
import com.whenly.events.NodeStatusEvent;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Data
@Slf4j
@Service  // Managed by Spring
public class ErlangBackendAPI {

    // Represents the Erlang node for this Java backend
    private final OtpNode node;
    // Mailbox for sending/receiving messages to/from Erlang nodes
    private final OtpMbox mbox;
    // Executor service to process incoming messages concurrently
    private final ExecutorService executorService;
    // Thread-safe list of registered message handlers
    private final CopyOnWriteArrayList<ErlangMessageHandler> handlers;

    // Shared list (injected by Spring) for managing string values, e.g., node addresses
    @Autowired
    private SharedStringList sharedStringList;

    // Used to publish events within the application
    @Autowired
    private ApplicationEventPublisher eventPublisher;

    /**
     * Constructor for ErlangBackendAPI.
     * <p>
     * Initializes the Erlang node, mailbox, executor service, and starts a listener thread
     * to continuously receive messages from Erlang.
     * 
     * @throws Exception if an error occurs during initialization.
     */
    public ErlangBackendAPI() throws Exception {
        // Example configuration: node name and cookie can be externalized or injected via @Value
        String nodeName = "java_backend@10.2.1.11";
        String cookie = "whenly";
        this.node = new OtpNode(nodeName, cookie);
        // Create a mailbox with the name "java_backend_api"
        this.mbox = node.createMbox("java_backend_api");
        // Create a fixed thread pool with 5 threads for processing messages
        this.executorService = Executors.newFixedThreadPool(5);
        // Initialize the thread-safe list of message handlers
        this.handlers = new CopyOnWriteArrayList<>();

        // Start a listener thread to receive messages from the Erlang node
        Thread listener = new Thread(() -> {
            log.info("Listener thread started.");
            while (true) {
                try {
                    // Receive a message from the mailbox (blocking call)
                    OtpErlangObject msg = mbox.receive();
                    log.info("Received message: " + msg);
                    // Process the received message asynchronously
                    processMessage(msg);
                } catch (Exception e) {
                    log.error("Error in listener thread: ", e);
                }
            }
        });
        // Set the thread as a daemon so it does not prevent application shutdown
        listener.setDaemon(true);
        listener.start();

        log.info("Java backend API node started as " + node.node() + " with mailbox " + mbox.getName());
    }

    /**
     * Processes an incoming Erlang message.
     *
     * @param msg the received Erlang object message.
     */
    private void processMessage(OtpErlangObject msg) {
        log.info("Processing message: " + msg);
        // Submit the processing task to the executor service for asynchronous execution
        executorService.submit(() -> {
            try {
                if (msg instanceof OtpErlangTuple) {
                    OtpErlangTuple tuple = (OtpErlangTuple) msg;
                    // Check if the tuple contains exactly three elements
                    if (tuple.arity() == 3) {
                        // The first element indicates the message type
                        OtpErlangAtom type = (OtpErlangAtom) tuple.elementAt(0);
                        switch (type.atomValue()) {
                            case "node_status":
                                // Process a node status message
                                handleNodeStatus(tuple);
                                break;
                            case "final_solution":
                                log.info("Received final solution message" + tuple);
                                // Process a final solution message
                                handleFinalSolution(tuple);
                                break;
                            default:
                                log.error("Unknown message type: " + type.atomValue());
                        }
                    } else {
                        log.error("Unexpected tuple size: " + tuple.arity());
                    }
                } else {
                    log.error("Message is not a tuple: " + msg);
                }
                // Notify all registered handlers about the received message
                for (ErlangMessageHandler handler : handlers) {
                    handler.onMessageReceived(msg);
                }
            } catch (Exception e) {
                log.error("Error processing message: " + e.getMessage(), e);
            }
        });
    }

    /**
     * Handles messages of type 'node_status'.
     * Expected tuple format: {node_status, Node, up/down}
     *
     * @param tuple the Erlang tuple message.
     */
    private void handleNodeStatus(OtpErlangTuple tuple) {
        try {
            // Extract the node name and its status from the tuple
            OtpErlangAtom nodeAtom = (OtpErlangAtom) tuple.elementAt(1);
            OtpErlangAtom statusAtom = (OtpErlangAtom) tuple.elementAt(2);
            String nodeName = nodeAtom.atomValue();
            String status = statusAtom.atomValue();
            // Publish a NodeStatusEvent to inform other components about the node status
            eventPublisher.publishEvent(new NodeStatusEvent(nodeName, status));
        } catch (Exception e) {
            log.error("Error handling node_status message: " + e.getMessage(), e);
        }
    }

    /**
     * Handles messages of type 'final_solution'.
     * Expected tuple format: {final_solution, EventId, FinalSolutionList}
     *
     * @param tuple the Erlang tuple message.
     */
    private void handleFinalSolution(OtpErlangTuple tuple) {
        try {
            // Retrieve the event ID from the second element of the tuple
            OtpErlangObject eventIdObj = tuple.elementAt(1);
            String eventId;
            if (eventIdObj instanceof OtpErlangString) {
                eventId = ((OtpErlangString) eventIdObj).stringValue();
            } else if (eventIdObj instanceof OtpErlangAtom) {
                eventId = ((OtpErlangAtom) eventIdObj).atomValue();
            } else {
                throw new IllegalArgumentException("Unexpected type for eventId: " + eventIdObj.getClass().getName());
            }
            
            // The third element must be a list; verify and cast it accordingly
            OtpErlangObject finalSolutionObj = tuple.elementAt(2);
            if (!(finalSolutionObj instanceof OtpErlangList)) {
                throw new IllegalArgumentException("Final solution is not an OtpErlangList, but: " 
                        + finalSolutionObj.getClass().getName());
            }
            OtpErlangList finalSolution = (OtpErlangList) finalSolutionObj;
            
            System.out.println("Received final solution: " + finalSolution);
            System.out.println("Received eventId: " + eventId);
            
            // Execute the desired action, such as publishing an event or updating the database.
            // In this example, a FinalSolutionEvent is published.
            eventPublisher.publishEvent(new FinalSolutionEvent(eventId, finalSolution.toString()));
            
        } catch (Exception e) {
            System.err.println("Error handling final_solution message: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Sends a 'create_event' message to the Erlang backend.
     *
     * @param targetNode  the target Erlang node.
     * @param eventId     the event ID.
     * @param deadline    the deadline as a Unix timestamp.
     * @param constraints an Erlang list of constraints.
     */
    public void createEvent(String targetNode, String eventId, long deadline, OtpErlangList constraints) {
        try {
            // Prepare the message as an Erlang tuple with five elements
            OtpErlangObject[] args = new OtpErlangObject[5];
            args[0] = new OtpErlangAtom("create_event");
            args[1] = new OtpErlangAtom(targetNode);
            args[2] = new OtpErlangAtom(eventId);
            args[3] = new OtpErlangLong(deadline);
            args[4] = constraints;
            OtpErlangObject msgToSend = new OtpErlangTuple(args);
            // Send the message to the Erlang node via the mailbox
            mbox.send("erlang_backend_api", "erlang_backend@10.2.1.11", msgToSend);
        } catch (Exception e) {
            log.error("Error sending createEvent message: " + e.getMessage(), e);
        }
    }

    /**
     * Sends an 'add_constraint' message to the Erlang backend.
     *
     * @param targetNode     the target Erlang node.
     * @param eventId        the event ID.
     * @param newConstraints an Erlang list of new constraints.
     */
    public void addConstraint(String targetNode, String eventId, OtpErlangList newConstraints) {
        try {
            // Prepare the message as an Erlang tuple with the required elements
            OtpErlangObject[] args = new OtpErlangObject[]{
                new OtpErlangAtom("add_constraint"),
                new OtpErlangAtom(targetNode),
                new OtpErlangAtom(eventId),
                newConstraints
            };
            OtpErlangObject msgToSend = new OtpErlangTuple(args);
            // Send the message to the Erlang node via the mailbox
            mbox.send("erlang_backend_api", "erlang_backend@10.2.1.11", msgToSend);
        } catch (Exception e) {
            log.error("Error sending addConstraint message: " + e.getMessage(), e);
        }
    }

    /**
     * Registers a new Erlang message handler.
     *
     * @param handler the ErlangMessageHandler to add.
     */
    public void addHandler(ErlangMessageHandler handler) {
        handlers.add(handler);
    }
}
