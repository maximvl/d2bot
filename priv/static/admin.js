function post_team() {
  $.post('/admin/team/add', { name: $("#team_name").val() },
        function(data) {
          console.log(data);
        });
}

function get_teams() {
  $.get('/admin/teams', { start: 0, count: 20 },
        function(data) {
          console.log(data);
        });
}
