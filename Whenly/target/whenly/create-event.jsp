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
    <br>
    <a href="user_area.jsp"> <button> User Area </button></a>
  </div>
</div>

<%
  HttpSession sessionObj = request.getSession();
  String eventId = (String) sessionObj.getAttribute("eventId");

  if (eventId != null && !eventId.isEmpty()) {
    sessionObj.removeAttribute("eventId");
%>
<script>
  alert("Event created successfully! eventID: <%= eventId %>");
</script>
<%
  }
%>


</body>
</html>