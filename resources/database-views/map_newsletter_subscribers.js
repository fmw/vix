function(doc) {
    if(doc.type === "newsletter-subscriber") {
        emit([doc.language, doc.email], doc);
    }
}
