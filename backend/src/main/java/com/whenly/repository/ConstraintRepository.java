package com.whenly.repository;

import com.whenly.model.Event;
import com.whenly.model.Constraint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ConstraintRepository extends JpaRepository<Constraint, Long> {

    /**
     * Verifica se esistono vincoli per un determinato evento e utente.
     */
    boolean existsByEventIdAndUsername(Long eventId, String username);

    /**
     * Recupera tutti gli eventi collegati a un determinato utente.
     */
    @Query("SELECT c.event FROM Constraint c WHERE c.username = :username")
    List<Event> findEventsByUsername(@Param("username") String username);

    /**
     * Recupera tutti i vincoli assegnati a un nodo Erlang specifico che non hanno ancora un risultato finale.
     */
    @Query("SELECT c FROM Constraint c WHERE c.assignedErlangNode = :erlangNode")
    List<Constraint> findConstraintsByErlangNode(@Param("erlangNode") String erlangNode);

    /**
     * Recupera tutti i vincoli per un evento specifico.
     */
    @Query("SELECT c FROM Constraint c WHERE c.event.id = :eventId")
    List<Constraint> findConstraintsByEventId(@Param("eventId") Long eventId);
}
