<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta charset="UTF-8"/>
  <title>◊(select 'title metas)</title>
</head>
<body>

  <script id="leibniz-document" type="application/xml">
    ◊(let () (local-require xml) (parameterize ([empty-tag-shorthand 'always]) (->html (select* 'leibniz doc))))
  </script>

  <style type="text/css">

    * {
        box-sizing: border-box;
    }

    .column {
        float: left;
        padding: 10px;
    }
    .main {
      width: 80%;
    }
    .context {
      width: 20%;
    }
    .row:after {
        content: "";
        display: table;
        clear: both;
    }
    .LeibnizInput { background-color: #E8E8FF; }
    .LeibnizOutput { background-color: #E0FFE0; }
    .LeibnizComment { background-color: #FFE8E8; }
    .LeibnizError { color: #FF4040; }
    .LeibnizErrorMessage { color: white; background-color: #FF4040; margin: 2px; padding: 2px;}
  </style>

  <div class="row">
    <div class="column context"></div>
    <div class="column main">
      <h1>◊(select 'title metas)</h1>
      by <b>◊(select 'author metas)</b>
    </div>
  </div>
  ◊(->html (select* 'doc doc))

</body>
</html>
