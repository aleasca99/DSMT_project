package com.example.whenly;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Scanner;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@WebServlet("/EventServlet")
public class EventServlet extends HttpServlet {
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {


        String eventName = request.getParameter("event-name");
        String deadline = request.getParameter("deadline");

        if (deadline != null) {
            deadline = deadline.replace("T", " ");
        }


        HttpSession session = request.getSession();
        String username = (String) session.getAttribute("username");

        try {
            eventName = URLEncoder.encode(eventName, "UTF-8");
            deadline = URLEncoder.encode(deadline, "UTF-8");
            username = URLEncoder.encode(username, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }

        String apiUrl = "http://10.2.1.11:8080/api/events/create?eventName=" + eventName + "&deadline=" + deadline + "&username=" + username;

        System.out.println("API URL: " + apiUrl);

        URL url = new URL(apiUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
        conn.setDoOutput(true);


        int responseCode = conn.getResponseCode();
        System.out.println("Response Code: " + responseCode);

        // Variabili per memorizzare la risposta
        StringBuilder responseMessage = new StringBuilder();
        String eventId = null;

        // Leggi la risposta
        try (Scanner scanner = new Scanner(conn.getInputStream())) {
            while (scanner.hasNext()) {
                responseMessage.append(scanner.nextLine());
            }
        }

        // Se la risposta è OK, prova a estrarre l'eventId
        if (responseCode == HttpURLConnection.HTTP_CREATED) {
            // Supponiamo che la risposta sia in formato JSON: {"eventId": "3", "message": "Event created successfully", "assignedErlangNode": "event_server1@10.2.1.9"}
            // Estrai l'eventId dalla risposta (usando una libreria come org.json o simile)
            String responseStr = responseMessage.toString();
            int eventIdIndex = responseStr.indexOf("\"eventId\":");
            if (eventIdIndex != -1) {
                eventId = responseStr.substring(eventIdIndex + 10, responseStr.indexOf("\"", eventIdIndex + 10));
            }
        }

        // Aggiungi l'eventId alla richiesta
        request.setAttribute("eventId", eventId);
        request.setAttribute("responseMessage", responseMessage.toString());

        // Rimanda alla stessa pagina e passa i dati
        if (responseCode == HttpURLConnection.HTTP_CREATED) {
            // Se la creazione è andata a buon fine, rimani sulla stessa pagina con un messaggio di successo
            request.getRequestDispatcher("create-event.jsp").forward(request, response);
        } else {
            // In caso di errore, rimani sulla stessa pagina e visualizza il messaggio di errore
            request.setAttribute("errorMessage", responseMessage.toString());
            request.getRequestDispatcher("create-event.jsp").forward(request, response);
        }
    }
}


