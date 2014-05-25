// Somewhere within Node.js lives a module called "http", 
// and we can make use of it in our own code by requiring 
// it and assigning the result of the require to a local variable.
var http	= require("http");
var url		= require("url");


function start(route, handle) {
	function onRequest(request, response) {
		var pathname = url.parse(request.url).pathname;
		console.log("Request for " + pathname + " received.");

		var contents = route(handle, pathname);

		response.writeHead(200, {"Content-Type":"text/plain"});
		response.write(contents);
		response.end();
	}
	http.createServer(onRequest).listen(8888);
	console.log("Server has started.");
}

exports.start=start;
