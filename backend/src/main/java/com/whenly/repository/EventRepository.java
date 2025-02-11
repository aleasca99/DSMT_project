package com.whenly.repository;

import com.whenly.model.Event;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.List; // Add this import


public interface EventRepository extends JpaRepository<Event, Long> {
    
    List<Event> findByErlangNodeIpAndFinalResultIsNull(String erlangNodeIp);

    

}
