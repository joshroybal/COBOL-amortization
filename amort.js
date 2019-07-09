function AddStyle()
{  
   var stylesheet = document.getElementById("tableStyle");
   stylesheet.innerHTML = "<link rel='stylesheet' media='all' type='text/css' href='/includes/gradienttable.css'/>";

   var table = document.getElementById("amortTable");
   table.classList.add("gradienttable-right-justify");
}

window.onload = AddStyle;
