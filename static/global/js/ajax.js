
function ajax(url, data, success, error)
{
    var xhr = new XMLHttpRequest();
    xhr.open('POST', url);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var r;
            try {
                r = JSON.parse(xhr.responseText);
            } catch (e) {
                error(200, "Invaild json");
            }
            success(r);
        } else {
            error(xhr.status, xhr.responseText);
        }
    };
    xhr.send(JSON.stringify(data));
}
