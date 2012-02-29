function(doc) {
    if(doc.type === "feed") {
        emit([doc["default-document-type"],
              doc["language"],
              doc["name"]],
             doc);
    }
}
