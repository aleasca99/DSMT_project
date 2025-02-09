package com.whenly.service;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Interface for handling messages received from Erlang nodes.
 */
public interface ErlangMessageHandler {

    /**
     * Called when a message is received from the Erlang backend.
     *
     * @param message The received message.
     */
    void onMessageReceived(OtpErlangObject message);
}
