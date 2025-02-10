package com.whenly.events;

public class NodeStatusEvent {
    private final String nodeName;
    private final String status; // ad es. "up" o "down"

    public NodeStatusEvent(String nodeName, String status) {
        this.nodeName = nodeName;
        this.status = status;
    }

    public String getNodeName() {
        return nodeName;
    }

    public String getStatus() {
        return status;
    }
}
