<%@ page import="java.util.Map" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="java.net.URL" %>
<%@ page import="java.net.HttpURLConnection" %>
<%@ page import="java.io.BufferedReader" %>
<%@ page import="java.io.InputStreamReader" %>
<%@ page import="com.google.gson.Gson" %>
<%@ page import="java.lang.reflect.Type" %>
<%@ page import="com.google.gson.reflect.TypeToken" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>

<%
  session = request.getSession();

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
      BufferedReader br = new BufferedReader(new InputStreamReader((conn.getInputStream())));
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
%>

<!DOCTYPE html>
<html lang="it">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>User Area - Whenly</title>
  <link rel="stylesheet" href="css/homepage.css">
</head>
<body>


  <div class = "container">
    <div class ="user_container">
      <h1> Welcome  <%=username %></h1>
      <br>
      <h2> Create a new event </h2>
      <a href="create-event.jsp">
        <button> Add event </button>
      </a>
      <br>
      <br>
      <h2> Add constraint </h2>
      <form id="constraint-form" action="AddConstraintServlet" method="post">
        <input type="text" id="code" name="code" placeholder="Event code" required>
        <br>
        <br>
        <div id="date-container">
          <div class="date-group">
            <p class ="dates">Start date: </p>
            <input type="datetime-local" id="event_date1[]" name="event_date1[]" required>
            <br>
            <br>
            <p class ="dates"> End date: </p>
            <input type ="datetime-local" id="event_date2[]" name="event_date2[]" required>
            <hr>
          </div>
        </div>
        <br>
        <br>
        <button type="button" id="add-interval"> Add interval time </button>
        <br>
        <br>
        <button type="submit">Add</button>
      </form>
      <h2> Past event </h2>
      <div class="table">
        <table >
          <tr>
            <th>Event Name</th>
            <th>Creator</th>
            <th>Result</th>
          </tr>
          <% if (events != null && !events.isEmpty()) { %>
          <% for (Map<String, String> event : events) { %>
          <% String result = event.get("result"); %>
          <tr>
            <td><%= event.get("eventName") %></td>
            <td><%= event.get("creator") %></td>
            <% if (result == null){ %>
              <td> in progress </td>
            <% } else {%>
            <% result = result.replace("T", " "); %>
              <td> <%= result %></td>
            <% } %>
          </tr>
          <% } %>
          <% } else { %>
          <tr>
            <td colspan="3">No event</td>
          </tr>
          <% } %>
        </table>
      </div>



      <br>
      <br>
      <form action="LogoutServlet" method="GET">
        <button type="submit">Logout</button>
      </form>

    </div>
  </div>

  <script type="text/javascript" src="javascript/add_input.js"></script>
</body>
</html>