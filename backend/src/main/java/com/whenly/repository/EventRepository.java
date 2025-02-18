package com.whenly.repository;

import com.whenly.model.Event;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List; // Add this import


public interface EventRepository extends JpaRepository<Event, Long> {
    
    /**
     * Retrieves all events with a specific Erlang node IP address and a null final result.
     */
    @Query(value = "SELECT * FROM event WHERE erlang_node_ip = :erlangNodeIp AND final_result IS NULL", nativeQuery = true)
    List<Event> findByErlangNodeIpAndFinalResultIsNull(@Param("erlangNodeIp") String erlangNodeIp);

    

}
