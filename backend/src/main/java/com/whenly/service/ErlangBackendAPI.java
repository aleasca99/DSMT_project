package com.whenly.service;

import com.ericsson.otp.erlang.*;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


/**
 * Class for communicating with Erlang nodes and interacting with Spring services.
 */
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

@Data
@Slf4j
public class ErlangBackendAPI {

    private final OtpNode node;
    private final OtpMbox mbox;
    private final ExecutorService executorService;
    private final CopyOnWriteArrayList<ErlangMessageHandler> handlers;

    /**
     * Creates a Java OTP node and opens a mailbox.
     *
     * @param nodeName The fully qualified name for this Java node (e.g., "java_backend@yourhost").
     * @param cookie   The shared cookie used for distributed communication.
     * @throws Exception if the node cannot be created.
     */
    public ErlangBackendAPI(String nodeName, String cookie) throws Exception {
        this.node = new OtpNode(nodeName, cookie);
        this.mbox = node.createMbox("java_backend_api");
        this.executorService = Executors.newFixedThreadPool(5); // Thread pool for processing messages
        this.handlers = new CopyOnWriteArrayList<>();

        // Start a thread to listen for incoming messages
        System.out.println("Sto per startare il nuovo thread");
        Thread listener = new Thread(() -> {
            System.out.println("Sono dentro il thread");
            while (true) {
                try {
                    OtpErlangObject msg = mbox.receive();
                    System.out.println("Received message: " + msg);
                    //log
                    log.info("Received message: " + msg);
                    processMessage(msg);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
        listener.setDaemon(true);
        listener.start();

        System.out.println("Java backend API node started as " + node.node() + " with mailbox " + mbox.getName());
    }

    /**
     * Registers a new message handler (a Spring service).
     *
     * @param handler The handler to register.
     */
    public void registerHandler(ErlangMessageHandler handler) {
        handlers.add(handler);
    }

    /**
     * Unregisters a message handler.
     *
     * @param handler The handler to unregister.
     */
    public void unregisterHandler(ErlangMessageHandler handler) {
        handlers.remove(handler);
    }

    /**
     * Processes incoming messages and forwards them to registered handlers.
     *
     * @param msg The incoming message.
     */
    private void processMessage(OtpErlangObject msg) {
        System.out.println("Received message: " + msg);
        executorService.submit(() -> {
            for (ErlangMessageHandler handler : handlers) {
                handler.onMessageReceived(msg);
            }
        });
    }

    /**
     * Sends a create event request to the specified Erlang node.
     *
     * @param targetNode   The target Erlang node name (e.g., "erlang_node@yourhost").
     * @param eventId      The event identifier.
     * @param deadline     The deadline as a Unix timestamp (seconds).
     * @param constraints  An OtpErlangList representing the list of initial constraints.
     */
    public void createEvent(String targetNode, String eventId, long deadline, OtpErlangList constraints) {
        try {
            // Costruzione dell'array di argomenti
            OtpErlangObject[] args = new OtpErlangObject[5];
            args[0] = new OtpErlangAtom("create_event");
            args[1] = new OtpErlangAtom("target test"); // Usa il parametro targetNode
            args[2] = new OtpErlangString("id test");  // Usa il parametro eventId
            args[3] = new OtpErlangLong(11);  // Usa il parametro deadline
            args[4] = constraints; // Usa il parametro constraints direttamente
    
            // Creazione del messaggio come una tupla
            OtpErlangObject msgToSend = new OtpErlangTuple(args);
    
            // Invio del messaggio (supponendo che tu abbia una mailbox chiamata `mbox`)
            mbox.send("erlang_backend_api", "erlang_backend@10.2.1.9", msgToSend);
    
            System.out.println("Messaggio inviato al nodo Erlang con successo.");
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Errore durante l'invio del messaggio al nodo Erlang.");
        }
    }

    /**
     * Sends a constraint request to the specified Erlang node.
     *
     * @param targetNode     The target Erlang node name (e.g., "erlang_node@yourhost").
     * @param eventId        The event identifier.
     * @param newConstraints An OtpErlangList representing the list of new constraints.
     */
    public void addConstraint(String targetNode, String eventId, OtpErlangList newConstraints) {
        try {
            OtpErlangObject[] args = new OtpErlangObject[]{
                new OtpErlangAtom("erlang_backend_api"),
                new OtpErlangAtom("add_constraint"),
                new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom(eventId), newConstraints})
            };

            OtpErlangTuple rpcCall = new OtpErlangTuple(new OtpErlangObject[]{mbox.self(), new OtpErlangTuple(args)});

            mbox.send("erlang_backend_api", "erlang_backend@10.2.1.9", rpcCall);

            OtpErlangObject reply = mbox.receive();
            System.out.println("Received RPC reply: " + reply);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

        /**
     * Main method for testing purposes.
     * This method demonstrates sending a create event and a constraint request.
     */
    public static void main(String[] args) {
        try {
            System.out.println("Java STARTING");
            String javaNodeName = "java_backend@10.2.1.11";
            String cookie = "whenly";

            ErlangBackendAPI backendAPI = new ErlangBackendAPI(javaNodeName, cookie);

            System.out.println("Java STARTED");

            // Target Erlang Event Server node name.
            String targetErlangNode = "event_node1@yourhost"; // Replace with your Erlang node's name.

            // Example event ID and deadline.
            String eventId = "my_event";
            long deadline = (System.currentTimeMillis() / 1000) + 60; // 60 seconds from now

            // Build an example constraint list.
            // Each interval is represented as a tuple: {Start, End}.
            OtpErlangObject[] intervalTuple = new OtpErlangObject[] {
                new OtpErlangLong(1600000000),
                new OtpErlangLong(1610000000)
            };
            OtpErlangTuple interval = new OtpErlangTuple(intervalTuple);
            OtpErlangList constraints = new OtpErlangList(new OtpErlangObject[] { interval });

            // Send a create event request.
            backendAPI.createEvent(targetErlangNode, eventId, deadline, constraints);

            System.out.println("finita CreateEvent");

            // Build a new constraint request.
            OtpErlangObject[] newIntervalTuple = new OtpErlangObject[] {
                new OtpErlangLong(1605000000),
                new OtpErlangLong(1615000000)
            };
            OtpErlangTuple newInterval = new OtpErlangTuple(newIntervalTuple);
            OtpErlangList newConstraints = new OtpErlangList(new OtpErlangObject[] { newInterval });

            // Send a constraint request.
            backendAPI.addConstraint(targetErlangNode, eventId, newConstraints);

            // The node will keep running and printing any incoming messages.
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
