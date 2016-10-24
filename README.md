# cl-ipfs-api
[![Build Status](https://travis-ci.org/wemeetagain/cl-ipfs-api.svg?branch=master)](https://travis-ci.org/wemeetagain/cl-ipfs-api)
[![Coverage Status](https://coveralls.io/repos/wemeetagain/cl-ipfs-api/badge.svg?branch=master&service=github)](https://coveralls.io/github/wemeetagain/cl-ipfs-api?branch=master)

A client library for the IPFS API.

## Examples

## Documentation

### [CL-IPFS-API](#CL-IPFS-API) (IPFS-API)

##### [special] [\***USER-AGENT**\*](#CL-IPFS-API:*USER-AGENT*)

    Identifying user-agent string for API calls

##### [special] [\***HOST**\*](#CL-IPFS-API:*HOST*)

    Hostname for API calls

##### [special] [\***ENCODING**\*](#CL-IPFS-API:\*ENCODING\*)

    Default encoding type

##### [special] [\***PORT**\*](#CL-IPFS-API:*PORT*)

    Port for all API calls

##### [special] [\***API-PATH**\*](#CL-IPFS-API:*API-PATH*)

    Path prefix for all API calls

##### [macro] [**WITH-IPFS-CONNECTION**](#CL-IPFS-API:WITH-IPFS-CONNECTION) (&REST ARGS)


##### [function] [**ADD**](#CL-IPFS-API:ADD) (PATH &KEY RECURSIVE QUIET PROGRESS TRICKLE ONLY-HASH WRAP-WITH-DIRECTORY HIDDEN CHUNKER TIMEOUT WANT-STREAM)

    Add an object to ipfs.

##### [function] [**CAT**](#CL-IPFS-API:CAT) (IPFS-PATH &KEY TIMEOUT WANT-STREAM)

    Show IPFS object data.

##### [function] [**LS**](#CL-IPFS-API:LS) (IPFS-PATH &KEY HEADERS (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List links from an object.

##### [function] [**FILE-LS**](#CL-IPFS-API:FILE-LS) (IPFS-PATH &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List directory contents for Unix-filesystem objects.

##### [function] [**BLOCK-GET**](#CL-IPFS-API:BLOCK-GET) (KEY &KEY TIMEOUT WANT-STREAM)

    Get a raw IPFS block

##### [function] [**BLOCK-PUT**](#CL-IPFS-API:BLOCK-PUT) (DATA &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Store input as an IPFS block.

##### [function] [**BLOCK-STAT**](#CL-IPFS-API:BLOCK-STAT) (KEY &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Print information of a raw IPFS block.

##### [function] [**OBJECT-GET**](#CL-IPFS-API:OBJECT-GET) (KEY &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Get and serialize the DAG node named by KEY.

##### [function] [**OBJECT-PUT**](#CL-IPFS-API:OBJECT-PUT) (DATA &KEY INPUTENC (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    stores input as a DAG object, outputs its key.

##### [function] [**OBJECT-STAT**](#CL-IPFS-API:OBJECT-STAT) (KEY &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Get stats for the DAG node named by KEY.

##### [function] [**OBJECT-DATA**](#CL-IPFS-API:OBJECT-DATA) (KEY &KEY TIMEOUT WANT-STREAM)

    Output the raw bytes in an IPFS object.

##### [function] [**OBJECT-LINKS**](#CL-IPFS-API:OBJECT-LINKS) (KEY &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Output the links pointed to by the specified object.

##### [function] [**OBJECT-NEW**](#CL-IPFS-API:OBJECT-NEW) (TEMPLATE &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Create a new object from an ipfs template.

##### [function] [**OBJECT-PATCH**](#CL-IPFS-API:OBJECT-PATCH) (ROOT COMMAND ARGS &KEY CREATE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Create a new DAG node based on an existing one.

##### [function] [**PIN-ADD**](#CL-IPFS-API:PIN-ADD) (IPFS-PATH &KEY RECURSIVE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Pin objects to local storage.

##### [function] [**PIN-RM**](#CL-IPFS-API:PIN-RM) (IPFS-PATH &KEY RECURSIVE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Unpin an object from local storage.

##### [function] [**PIN-LS**](#CL-IPFS-API:PIN-LS) (&KEY TYPE COUNT QUIET (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List objects pinned to local storage.

##### [function] [**REPO-GC**](#CL-IPFS-API:REPO-GC) (&KEY QUIET (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Perform a garbage collection sweep on non-pinned objects.

##### [function] [**SWARM-ADDRS**](#CL-IPFS-API:SWARM-ADDRS) (&KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List known addresses.

##### [function] [**SWARM-PEERS**](#CL-IPFS-API:SWARM-PEERS) (&KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List peers with open connections.

##### [function] [**SWARM-CONNECT**](#CL-IPFS-API:SWARM-CONNECT) (ADDRESS &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Open connection to a given address.

##### [function] [**SWARM-DISCONNECT**](#CL-IPFS-API:SWARM-DISCONNECT) (ADDRESS &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Close connection to a given address.

##### [function] [**SWARM-FILTERS**](#CL-IPFS-API:SWARM-FILTERS) (&KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List currently applied filters.

##### [function] [**SWARM-FILTERS-ADD**](#CL-IPFS-API:SWARM-FILTERS-ADD) (ADDRESS &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Add an address filter.

##### [function] [**SWARM-FILTERS-RM**](#CL-IPFS-API:SWARM-FILTERS-RM) (ADDRESS &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Remove an address filter.

##### [function] [**BOOTSTRAP**](#CL-IPFS-API:BOOTSTRAP) (&KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Show peers in the bootstrap list.

##### [function] [**BOOTSTRAP-ADD**](#CL-IPFS-API:BOOTSTRAP-ADD) (PEERS &KEY DEFAULT (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Add peers to the bootstrap list.

##### [function] [**BOOTSTRAP-RM**](#CL-IPFS-API:BOOTSTRAP-RM) (PEERS &KEY ALL (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Remove peers from the bootstrap list.

##### [function] [**REFS**](#CL-IPFS-API:REFS) (IPFS-PATH &KEY FORMAT EDGES UNIQUE RECURSIVE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List links (references) from an object.

##### [function] [**REFS-LOCAL**](#CL-IPFS-API:REFS-LOCAL) (&KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    List all local references.

##### [function] [**RESOLVE**](#CL-IPFS-API:RESOLVE) (NAME &KEY RECURSIVE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Resolve the value of names to IPFS.

##### [function] [**NAME-PUBLISH**](#CL-IPFS-API:NAME-PUBLISH) (NAME IPFS-PATH &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Publish an object to IPNS.

##### [function] [**NAME-RESOLVE**](#CL-IPFS-API:NAME-RESOLVE) (NAME &KEY RECURSIVE WANT-STREAM)

    Get the value currently published at an IPNS name.

##### [function] [**DNS**](#CL-IPFS-API:DNS) (DOMAIN-NAME &KEY RECURSIVE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    DNS link resolver.


##### [function] [**DHT-QUERY**](#CL-IPFS-API:DHT-QUERY) (PEER-ID &KEY VERBOSE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Run a 'FindClosestPeers' query through the DHT.

##### [function] [**DHT-GET**](#CL-IPFS-API:DHT-GET) (KEY &KEY VERBOSE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Run a 'GetValue' query through the DHT.

##### [function] [**DHT-PUT**](#CL-IPFS-API:DHT-PUT) (KEY VALUE &KEY VERBOSE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Run a 'PutValue' query through the DHT.

##### [function] [**DHT-FINDPEER**](#CL-IPFS-API:DHT-FINDPEER) (PEER-ID &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Run a 'FindPeer' query through the DHT.

##### [function] [**DHT-FINDPROVS**](#CL-IPFS-API:DHT-FINDPROVS) (KEY &KEY VERBOSE (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Run a 'FindProviders' query through the DHT.

##### [function] [**PING**](#CL-IPFS-API:PING) (PEER-ID &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Send echo request packets to IPFS hosts.

##### [function] [**CONFIG**](#CL-IPFS-API:CONFIG) (KEY VALUE &KEY BOOL JSON (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Get and set IPFS config values.

##### [function] [**CONFIG-SHOW**](#CL-IPFS-API:CONFIG-SHOW) (&KEY TIMEOUT WANT-STREAM)

    Outputs the content of the config file.

##### [function] [**CONFIG-REPLACE**](#CL-IPFS-API:CONFIG-REPLACE) (FILE &KEY (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Replace the config with FILE.

##### [function] [**ID**](#CL-IPFS-API:ID) (&KEY FORMAT (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Show IPFS Node ID information

##### [function] [**VERSION**](#CL-IPFS-API:VERSION) (&KEY NUMBER (ENCODING \*ENCODING\*) TIMEOUT WANT-STREAM)

    Show IPFS version information.

## Testing

```lisp
(asdf:test-system :cl-ipfs-api)
```

## License

MIT
