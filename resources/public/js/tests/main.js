test("incrementSlug", function() {
    equals(incrementSlug("/blog/foo"), "/blog/foo-2");
    equals(incrementSlug("/blog/foo-2"), "/blog/foo-3");
    equals(incrementSlug("/blog/foo-9"), "/blog/foo-10");
    equals(incrementSlug("/blog/foo-123456789"), "/blog/foo-123456790");
    equals(incrementSlug(""), "-2");
});


test("invalidSlug", function() {
    var alphaNumericError = "Slugs can only contain '/', '-' " +
                            "and alphanumeric characters.";
    var consError =         "Consecutive '-' or '/' characters are not " +
                            "allowed in the slug.";


    equals(invalidSlug("/blog/foo"), false);
    equals(invalidSlug("/blog/foo-2"), false);
    equals(invalidSlug("/blog/foo-2/f00"), false);

    equals(invalidSlug("/blog/fó0"), alphaNumericError);
    equals(invalidSlug("/blog/foo!"), alphaNumericError);
    equals(invalidSlug("/blog/foo#"), alphaNumericError);
    equals(invalidSlug("/blog/foo@"), alphaNumericError);
    equals(invalidSlug("/blog/foo%"), alphaNumericError);
    equals(invalidSlug("/blog/foo%"), alphaNumericError);
    equals(invalidSlug("%"), alphaNumericError);
    equals(invalidSlug("2%h"), alphaNumericError);

    equals(invalidSlug("/blog//foo"), consError);
    equals(invalidSlug("/blog/foo--2"), consError);
    equals(invalidSlug("/blog/foo-bar-2/x--"), consError);
});

test("createSlug", function() {
    equals(createSlug("hello, cruel world!"), "/blog/hello-cruel-world");
    equals(createSlug("foo---bar"), "/blog/foo-bar");

    equals(createSlug(1), "");

});

test("Document model", function() {
    equals((new Document()).url(), "/json/blog/new");
    equals((new Document({id: "/blog/foo"})).url(), "/json/document/blog/foo");
});
