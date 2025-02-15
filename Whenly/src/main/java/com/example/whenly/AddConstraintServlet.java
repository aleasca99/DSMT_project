package com.example.whenly;

import com.google.gson.Gson;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

@WebServlet("/AddConstraintServlet")
public class AddConstraintServlet extends HttpServlet {

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // Recupera i parametri dal form
        String eventId = request.getParameter("code");
        String username = (String) request.getSession().getAttribute("username"); // Prende l'username dalla sessione

        if (eventId == null || username == null) {
            response.sendRedirect("user-area.jsp?error=Missing+event+ID+or+username");
            return;
        }

        // Recupera tutti gli intervalli di tempo dal form
        String[] startDates = request.getParameterValues("event_date1[]");
        String[] endDates = request.getParameterValues("event_date2[]");

        if (startDates == null || endDates == null || startDates.length != endDates.length) {
            response.sendRedirect("user-area.jsp?error=Invalid+date+format");
            return;
        }

        // Creazione della lista di constraints nel formato richiesto
        List<String> constraints = new ArrayList<>();
        for (int i = 0; i < startDates.length; i++) {
            String startDate = startDates[i].replace("T", " ");
            String endDate = endDates[i].replace("T", " ");
            constraints.add(startDate + "," + endDate);
        }

        // Converte la lista in JSON
        Gson gson = new Gson();
        String jsonBody = gson.toJson(constraints);

        // URL dell'API REST
        String apiUrl = "http://10.2.1.11:8080/api/constraints/add?eventId=" + eventId + "&username=" + username;

        // Configura la richiesta POST
        URL url = new URL(apiUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setDoOutput(true);

        // Scrive il corpo JSON
        try (OutputStream os = conn.getOutputStream()) {
            byte[] input = jsonBody.getBytes("utf-8");
            os.write(input, 0, input.length);
        }


        // Legge la risposta
        int responseCode = conn.getResponseCode();
        System.out.println("Response Code: " + responseCode);
        if (responseCode == HttpURLConnection.HTTP_CREATED) {
            response.sendRedirect("user_area.jsp?success=Constraints+added+successfully");
        } else {
            // Legge il messaggio di errore dall'API
            StringBuilder responseMessage = new StringBuilder();
            try (BufferedReader br = new BufferedReader(new java.io.InputStreamReader(conn.getErrorStream(), "utf-8"))) {
                String responseLine;
                while ((responseLine = br.readLine()) != null) {
                    responseMessage.append(responseLine.trim());
                }
            }
            response.sendRedirect("user_area.jsp?error=" + responseMessage.toString());
        }
    }
}
