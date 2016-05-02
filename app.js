function uploadFile(e) {
    var file = document.getElementById("file").files[0];
    var fileUploadButton = document.getElementById("file-upload-button");
    fileUploadButton.disabled = true;
    var request = new XMLHttpRequest();
    request.onerror = function(e) {
	fileUploadButton.disabled = false;
	window.alert("Upload of file " + file.name + " failed.");
    };
    request.onload = function(e) {
	fileUploadButton.disabled = false;
	var status = JSON.parse(request.responseText);
	window.alert("Upload of file "
		     + file.name +
		     (status.successful? " succeeded " : " failed "));
    };

    request.open("post", "/train", true);
    request.setRequestHeader("Content-Type", "multipart/form-data");
    request.setRequestHeader("X-File-Name", file.name);
    request.setRequestHeader("X-File-Size", file.size);
    request.setRequestHeader("X-File-Type", file.type);

    request.send(file);
}

function generate() {
    var formData = new FormData(document.getElementById("generate-form"));    
    var request = new XMLHttpRequest();

    var contentBox = document.getElementById("poems-box");
    var poemBox = document.createElement("article");
    poemBox.className = "poem-container";
    var textContainer = document.createElement("div");
    textContainer.className = "text-wrapper";
    var loadingText = document.createTextNode("Generating, please wait...");
    textContainer.appendChild(loadingText);
    poemBox.appendChild(textContainer);
    
    if (contentBox.childNodes.length > 0) {
	contentBox.insertBefore(poemBox, contentBox.childNodes[0]);
    } else {
	contentBox.appendChild(poemBox);
    }

    request.onerror = function(e) {
	contentBox.removeChild(poemBox);
	window.alert("Generation failed");
    }

    request.onload = function(e) {
	var response = JSON.parse(request.responseText);
	if (!response.successful) {
	    contentBox.removeChild(poemBox);
	    window.alert("Generation failed");
	} else {
	    var lines = response.text.split("\n");
	    var seed = response.seed;
	    var seedField = document.createElement("h2");
	    seedField.appendChild(document.createTextNode(seed));	    
	    textContainer.removeChild(loadingText);
	    textContainer.appendChild(seedField);
	    lines.forEach(function(line) {
		textContainer.appendChild(document.createTextNode(line));
		textContainer.appendChild(document.createElement("br"));
	    });
	}
    }

    var vals = [];
    for (let entry of formData.entries()) {
	vals.push(escape(entry[0]) + "=" + escape(entry[1]));
    }

    var paramString = vals.join([separator = "&"]);
    
    request.open("GET", "/generate?" + paramString, true);
    request.timeout = 0;
    request.send();
}
