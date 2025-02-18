package com.whenly.events;

public class NodeStatusEvent {
    // The name of the node
    private final String nodeName;
    
    // The status of the node, e.g., "up" or "down"
    private final String status;

    // Constructor to initialize the nodeName and status
    public NodeStatusEvent(String nodeName, String status) {
        this.nodeName = nodeName;
        this.status = status;
    }

    // Getter method for nodeName
    public String getNodeName() {
        return nodeName;
    }

    // Getter method for status
    public String getStatus() {
        return status;
    }
}