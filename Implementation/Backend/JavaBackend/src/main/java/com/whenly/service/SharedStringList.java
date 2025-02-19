package com.whenly.service;

import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

public class SharedStringList {

    // Using a Set instead of a List to automatically avoid duplicate entries
    private final Set<String> stringSet;
    // Lock to ensure thread-safe access to the stringSet
    private final ReentrantLock lock;

    /**
     * Constructor for SharedStringList.
     * Initializes the HashSet and the ReentrantLock.
     */
    public SharedStringList() {
        this.stringSet = new HashSet<>();
        // Optionally, pre-populate the set with initial nodes if needed
        this.lock = new ReentrantLock();
    }

    /**
     * Adds a string value to the set in a thread-safe manner.
     * 
     * @param value the string to be added.
     */
    public void addString(String value) {
        lock.lock();
        try {
            // The HashSet automatically ignores duplicates
            stringSet.add(value);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Removes a string value from the set in a thread-safe manner.
     * 
     * @param value the string to be removed.
     */
    public void removeString(String value) {
        lock.lock();
        try {
            stringSet.remove(value);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Returns a copy of the current set of strings.
     * The returned list is a snapshot, ensuring external modifications do not affect the original set.
     * 
     * @return a List containing the current strings.
     */
    public List<String> getStrings() {
        lock.lock();
        try {
            return new ArrayList<>(stringSet);
        } finally {
            lock.unlock();
        }
    }
}
