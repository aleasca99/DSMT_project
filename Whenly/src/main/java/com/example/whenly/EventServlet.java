package com.example.whenly;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Scanner;

import org.json.JSONObject;

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

        // Variable to save response message
        StringBuilder responseMessage = new StringBuilder();
        String eventId = null;

        // read json response
        try (Scanner scanner = new Scanner(conn.getInputStream())) {
            while (scanner.hasNext()) {
                responseMessage.append(scanner.nextLine());
            }
        }


        if (responseCode == HttpURLConnection.HTTP_CREATED) {
            String responseStr = responseMessage.toString();
            try {
                JSONObject jsonResponse = new JSONObject(responseStr);
                eventId = jsonResponse.optString("eventId", null); // Ottieni eventId in modo sicuro
            } catch (Exception e) {
                System.err.println("Errore nel parsing della risposta JSON: " + e.getMessage());
            }
        }



        // storage event id in the sesseion
        session.setAttribute("eventId", eventId);


        response.sendRedirect("create-event.jsp");
    }
}
