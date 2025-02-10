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
@Service  // Ora gestito da Spring
public class ErlangBackendAPI {

    private final OtpNode node;
    private final OtpMbox mbox;
    private final ExecutorService executorService;
    private final CopyOnWriteArrayList<ErlangMessageHandler> handlers;

    @Autowired
    private SharedStringList sharedStringList;

    @Autowired
    private ApplicationEventPublisher eventPublisher; // Per pubblicare eventi

    // Costruttore – Nota: i parametri possono essere externalizzati o gestiti in altro modo
    public ErlangBackendAPI() throws Exception {
        // Esempio di configurazione; puoi utilizzare valori esterni o @Value
        String nodeName = "java_backend@10.2.1.11";
        String cookie = "whenly";
        this.node = new OtpNode(nodeName, cookie);
        this.mbox = node.createMbox("java_backend_api");
        this.executorService = Executors.newFixedThreadPool(5);
        this.handlers = new CopyOnWriteArrayList<>();

        // Avvio del thread per l'ascolto dei messaggi
        Thread listener = new Thread(() -> {
            log.info("Listener thread started.");
            while (true) {
                try {
                    OtpErlangObject msg = mbox.receive();
                    log.info("Received message: " + msg);
                    processMessage(msg);
                } catch (Exception e) {
                    log.error("Error in listener thread: ", e);
                }
            }
        });
        listener.setDaemon(true);
        listener.start();

        log.info("Java backend API node started as " + node.node() + " with mailbox " + mbox.getName());
    }

    private void processMessage(OtpErlangObject msg) {
        log.info("Processing message: " + msg);
        executorService.submit(() -> {
            try {
                if (msg instanceof OtpErlangTuple) {
                    OtpErlangTuple tuple = (OtpErlangTuple) msg;
                    if (tuple.arity() == 3) {
                        OtpErlangAtom type = (OtpErlangAtom) tuple.elementAt(0);
                        switch (type.atomValue()) {
                            case "node_status":
                                handleNodeStatus(tuple);
                                break;
                            case "final_solution":
                                log.info("Received final solution message");
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
                // Notifica altri handler registrati se necessario
                for (ErlangMessageHandler handler : handlers) {
                    handler.onMessageReceived(msg);
                }
            } catch (Exception e) {
                log.error("Error processing message: " + e.getMessage(), e);
            }
        });
    }

    private void handleNodeStatus(OtpErlangTuple tuple) {
        try {
            // Estrae il nodo e lo stato: {node_status, Node, up/down}
            OtpErlangAtom nodeAtom = (OtpErlangAtom) tuple.elementAt(1);
            OtpErlangAtom statusAtom = (OtpErlangAtom) tuple.elementAt(2);
            String nodeName = nodeAtom.atomValue();
            String status = statusAtom.atomValue();
            // Pubblica l'evento di stato nodo
            eventPublisher.publishEvent(new NodeStatusEvent(nodeName, status));
        } catch (Exception e) {
            log.error("Error handling node_status message: " + e.getMessage(), e);
        }
    }

    private void handleFinalSolution(OtpErlangTuple tuple) {
        try {
            // Estrazione: {final_solution, EventId, FinalSolution}
            OtpErlangString eventId = (OtpErlangString) tuple.elementAt(1);
            OtpErlangList finalSolution = (OtpErlangList) tuple.elementAt(2);
            log.info("Received final solution: " + finalSolution);
            log.info("Received EventId: " + eventId);
            // Pubblica l'evento per la final solution
            eventPublisher.publishEvent(new FinalSolutionEvent(eventId.stringValue(), finalSolution.toString()));
        } catch (Exception e) {
            log.error("Error handling final_solution message: " + e.getMessage(), e);
        }
    }

    // I metodi createEvent e addConstraint rimangono invariati
    public void createEvent(String targetNode, String eventId, long deadline, OtpErlangList constraints) {
        try {
            OtpErlangObject[] args = new OtpErlangObject[5];
            args[0] = new OtpErlangAtom("create_event");
            args[1] = new OtpErlangAtom(targetNode);
            args[2] = new OtpErlangString(eventId);
            args[3] = new OtpErlangLong(deadline);
            args[4] = constraints;
            OtpErlangObject msgToSend = new OtpErlangTuple(args);
            mbox.send("erlang_backend_api", "erlang_backend@10.2.1.9", msgToSend);
        } catch (Exception e) {
            log.error("Error sending createEvent message: " + e.getMessage(), e);
        }
    }

    public void addConstraint(String targetNode, String eventId, OtpErlangList newConstraints) {
        try {
            OtpErlangObject[] args = new OtpErlangObject[]{
                new OtpErlangAtom("add_constraint"),
                new OtpErlangAtom(targetNode),
                new OtpErlangAtom(eventId),
                newConstraints
            };
            OtpErlangObject msgToSend = new OtpErlangTuple(args);
            mbox.send("erlang_backend_api", "erlang_backend@10.2.1.9", msgToSend);
        } catch (Exception e) {
            log.error("Error sending addConstraint message: " + e.getMessage(), e);
        }
    }

    public void addHandler(ErlangMessageHandler handler) {
        handlers.add(handler);
    }
}
