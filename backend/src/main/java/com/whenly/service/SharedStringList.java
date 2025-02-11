package com.whenly.service;

import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;


public class SharedStringList {

    private final Set<String> stringSet; // Cambiato da List a Set per evitare duplicati
    private final ReentrantLock lock;

    public SharedStringList() {
        this.stringSet = new HashSet<>();
        //aggiungi i nodi all'hashset
      
        
        this.lock = new ReentrantLock();
    }

    public void addString(String value) {
        lock.lock();
        try {
            stringSet.add(value); // HashSet gestisce automaticamente i duplicati
        } finally {
            lock.unlock();
        }
    }

    public void removeString(String value) {
        lock.lock();
        try {
            stringSet.remove(value);
        } finally {
            lock.unlock();
        }
    }

    public List<String> getStrings() {
        lock.lock();
        try {
            return new ArrayList<>(stringSet); // Ritorna una copia per garantire immutabilità
        } finally {
            lock.unlock();
        }
    }
}
