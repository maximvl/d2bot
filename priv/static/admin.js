function post_team() {
  $.post('/team/add', { name: $("#team_name").val() },
        function(data) {
          console.log(data);
        });
}
