function init() {
}

function run_command(func) {
  hide_result();
  func();
}

function hide_result() {
  $('#result').fadeOut('slow');
}

function show_result(res) {
  $('#result').text(JSON.stringify(res));
  $('#result').fadeIn('slow');
}

function add_team() {
  run_command(function() {
    $.post('/admin/team/add', { name: $("#add_team").val() },
           function(data) {
             show_result(data);
           });
  });
}

function get_teams() {
  run_command(function() {
    $.get('/admin/teams', { start: 0, count: 20 },
          function(data) {
            var table = "<table border=1 rules=all cellpadding=10><tr><th>ID<th>Name";
            data.teams.forEach(function(team) {
              table += '<tr><td>' + team.id + '<td>' + escape(team.name);
            });
            table += '</table>';
            $('#teams').html(table);
          });
  });
}

function delete_team() {
  run_command(function() {
    $.post('/admin/team/del', { name: $("#del_team").val() },
           function(data) {
             show_result(data);
           });
  });
}
