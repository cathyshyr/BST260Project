var url ='https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=8&season=2016&month=0&season1=2016&ind=0&team=9&rost=0&age=0';
var page = new WebPage()
var fs = require('fs');


page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}
