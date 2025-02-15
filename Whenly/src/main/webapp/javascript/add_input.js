document.getElementById("add-interval").addEventListener("click", function () {
    const dateContainer = document.getElementById("date-container");


    const newDateGroup = document.createElement("div");
    newDateGroup.classList.add("date-group");
    newDateGroup.innerHTML = `
        <br>
        <p class="dates"> Start date: </p>
        <br>
        <input type="datetime-local" name="event_date1[]" required>
        <br>
        <br>
        <p class="dates"> End date: </p>
        <br>
        <input type="datetime-local" name="event_date2[]" required>
        <br>
        <br>
        <button type="button" class="remove-interval">Remove</button>
        <hr>
        <br>
        <br>
      `;

    dateContainer.appendChild(newDateGroup);


    newDateGroup.querySelector(".remove-interval").addEventListener("click", function () {
        dateContainer.removeChild(newDateGroup);
    });
});