<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'title metas)</title>
</head>
<body>

  <script if="leibniz-contexts" type="application/xml">
    ◊(let () (local-require xml) (parameterize ([empty-tag-shorthand 'always]) (->html (select* 'leibniz doc))))
  </script>

  ◊(->html (select* 'doc doc))

</body>
</html>
