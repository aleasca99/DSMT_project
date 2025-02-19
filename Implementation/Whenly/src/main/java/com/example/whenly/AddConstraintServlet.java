package com.example.whenly;

import com.google.gson.Gson;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

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
        // retrieves parameters from the form
        String eventId = request.getParameter("code");
        HttpSession session = request.getSession();
        String username = (String) session.getAttribute("username");


        // Retrieves all time intervals from form
        String[] startDates = request.getParameterValues("event_date1[]");
        String[] endDates = request.getParameterValues("event_date2[]");


        // Create a list of constraints in request format
        List<String> constraints = new ArrayList<>();
        for (int i = 0; i < startDates.length; i++) {
            String startDate = startDates[i].replace("T", " ");
            String endDate = endDates[i].replace("T", " ");
            constraints.add(startDate + "," + endDate);
        }

        // Convert list to json
        Gson gson = new Gson();
        String jsonBody = gson.toJson(constraints);

        // API REST URL
        String apiUrl = "http://10.2.1.11:8080/api/constraints/add?eventId=" + eventId + "&username=" + username;

        // Configure POST request
        URL url = new URL(apiUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setDoOutput(true);

        // Write Json body
        try (OutputStream os = conn.getOutputStream()) {
            byte[] input = jsonBody.getBytes("utf-8");
            os.write(input, 0, input.length);
        }


        // Read the response
        int responseCode = conn.getResponseCode();
        System.out.println("Response Code: " + responseCode);
        if (responseCode == HttpURLConnection.HTTP_CREATED) {
            session.setAttribute("message", "constraints added successfully");
            response.sendRedirect("userEvents");

        } else {
            // Read error message
            StringBuilder responseMessage = new StringBuilder();
            try (BufferedReader br = new BufferedReader(new java.io.InputStreamReader(conn.getErrorStream(), "utf-8"))) {
                String responseLine;
                while ((responseLine = br.readLine()) != null) {
                    responseMessage.append(responseLine.trim());
                }
            }
            session.setAttribute("message", responseMessage.toString());
            response.sendRedirect("userEvents");
        }
    }
}
