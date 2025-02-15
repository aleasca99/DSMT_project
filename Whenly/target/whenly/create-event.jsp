<%--
  Created by IntelliJ IDEA.
  User: aleasca
  Date: 10/02/25
  Time: 16:00
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<!DOCTYPE html>
<html lang="it">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Create Event - Whenly</title>
  <link rel="stylesheet" href="css/homepage.css">
</head>
<body>

<div class="container">
  <div class="event-container">
    <h2>Create a new event</h2>
    <form id="create-event-form" action="EventServlet" method="post">
      <input type="text" id="event-name" name="event-name" placeholder="Name Event" required>
      <br>
      <br>
      <input type="datetime-local" id="deadline" name="deadline" required>
      <br>
      <br>
      <button type="submit">Create Event</button>
    </form>
  </div>
  <a href="user_area.jsp"> <button> User Area</button></a>
</div>

<script type="text/javascript" src="javascript/create-event.js"></script>

</body>
</html>