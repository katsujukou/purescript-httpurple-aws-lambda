import { createServer, request } from "node:http";
import url from "node:url"
// Create a local server to receive data from
const server = createServer();

// Listen to the request event
server.on('request', (req, res) => {
    let body = '';
    req.on("data", (chunk) => {
        body += chunk;
    });
    req.on("end", () => {
        console.log("[on request]", body)
        const json = JSON.parse(body);
        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({
            data: `Hello, ${json.name}!`
        }));
    });
});


const fetch = (data) => new Promise((resolve) => {
    server.listen(0, () => {
        let { address, port } = server.address()
        const req = request({
            hostname: address,
            port: port,
            path: '/',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
        });
        req.on("response", (res) => {
            let responseBody = '';
            res.setEncoding('utf8');
            res.on('data', chunk => {
                responseBody += chunk
            })
            res.on('end', () => {
                server.close();
                resolve(responseBody);
            })
            console.log(res.httpVersion)
        });
        req.write(data)
        req.end()
    });

})

console.log("[main]", JSON.parse(await fetch('{ "name": "John" }')))