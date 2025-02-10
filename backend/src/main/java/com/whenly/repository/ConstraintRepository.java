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

    boolean existsByEventIdAndUsername(Long eventId, String username);

    @Query("SELECT c.event FROM Constraint c WHERE c.username = :username")
    List<Event> findEventsByUsername(@Param("username") String username);
}
