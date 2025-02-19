package com.example.whenly;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Type;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

@WebServlet("/userEvents")
public class UserEventsServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        HttpSession session = request.getSession(false);
        if (session == null || session.getAttribute("username") == null) {
            response.sendRedirect("index.jsp");
            return;
        }

        String username = (String) session.getAttribute("username");
        List<Map<String, String>> events = new ArrayList<>();

        try {
            String apiUrl = "http://10.2.1.11:8080/api/events/user/" + username;
            URL url = new URL(apiUrl);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setRequestProperty("Accept", "application/json");

            if (conn.getResponseCode() == 200) {
                BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
                StringBuilder responseJson = new StringBuilder();
                String line;
                while ((line = br.readLine()) != null) {
                    responseJson.append(line);
                }
                conn.disconnect();

                Gson gson = new Gson();
                Type eventListType = new TypeToken<ArrayList<Map<String, String>>>() {}.getType();
                events = gson.fromJson(responseJson.toString(), eventListType);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        // Set attributes for JSP
        request.setAttribute("events", events);
        request.setAttribute("username", username);

        // Forwards the request to the view JSP
        request.getRequestDispatcher("user_area.jsp").forward(request, response);
    }
}
