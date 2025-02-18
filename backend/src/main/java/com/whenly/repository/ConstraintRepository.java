package com.whenly.repository;

import com.whenly.model.Event;
import com.whenly.model.Constraint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigInteger;
import java.util.List;

@Repository
public interface ConstraintRepository extends JpaRepository<Constraint, Long> {

    /**
     * Checks if constraints exist for a specific event and user.
     */
    boolean existsByEventIdAndUsername(Long eventId, String username);

    /**
     * Retrieves all event IDs associated with a specific user.
     */
    @Query(value = "SELECT DISTINCT event_id FROM `constraints` WHERE username = :username", nativeQuery = true)
    List<BigInteger> findEventIdsByUsername(@Param("username") String username);

    /**
     * Retrieves all constraints assigned to a specific Erlang node that do not yet have a final result.
     */
    @Query("SELECT c FROM Constraint c WHERE c.assignedErlangNode = :erlangNode")
    List<Constraint> findConstraintsByErlangNode(@Param("erlangNode") String erlangNode);

    /**
     * Retrieves all constraints for a specific event.
     */
    @Query("SELECT c FROM Constraint c WHERE c.event.id = :eventId")
    List<Constraint> findConstraintsByEventId(@Param("eventId") Long eventId);
}