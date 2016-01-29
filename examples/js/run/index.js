document.getElementById('eval').addEventListener('click', function() {
  var text = document.getElementById('input').value;
  var output = LION.run(text);
  document.getElementById('output').value = output;
});
