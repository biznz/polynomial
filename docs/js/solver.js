
var getResult =()=>{ 
	console.log(" teste ");
	var result = document.getElementById("result");
	var node = document.createElement("p");
	var text = document.createTextNode("x = (-b +- sqrt(b^2-4ac))/(2a) .");
	node.appendChild(text);
	result.appendChild(node);
}