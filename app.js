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
