<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'title metas)</title>
</head>
<body>

  <script if="leibniz-contexts" type="application/xml">
    ◊(let () (local-require xml) (parameterize ([empty-tag-shorthand 'always]) (->html (select* 'leibniz doc))))
  </script>

  <style type="text/css">
    .LeibnizInput { background-color: #E8E8FF; }
    .LeibnizOutput { background-color: #E0FFE0; }
    .LeibnizComment { background-color: #FFE8E8; }
    .LeibnizError { color: #FF4040; }
    .LeibnizErrorMessage { color: white; background-color: #FF4040; margin: 2px; padding: 2px;}
  </style>

  ◊(->html (select* 'doc doc))

</body>
</html>
