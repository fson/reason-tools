type client;

type message;

type codeBlock;

external createClient : unit => client = "Client" [@@bs.module "discord.js"] [@@bs.new];

external login : client => string => unit = "" [@@bs.send];

external onReady : client => _ [@bs.as "ready"] => (unit => unit) => unit = "on" [@@bs.send];

external onMessage : client => _ [@bs.as "message"] => (message => unit) => unit =
  "on" [@@bs.send];

external getContent : message => string = "content" [@@bs.get];

external reply : message => string => unit = "" [@@bs.send];

external codeBlocks : string => array codeBlock = "gfm-code-blocks" [@@bs.module];

external getCode : codeBlock => string = "code" [@@bs.get];

let discordClient = createClient ();

let syntaxLang lang =>
  switch lang {
  | "ML" => "ocaml"
  | "RE"
  | _ => "rust"
  };

let contains s1 s2 =>
  try {
    let len = String.length s2;
    for i in 0 to (String.length s1 - len) {
      if (String.sub s1 i len == s2) {
        raise Exit
      }
    };
    false
  } {
  | Exit => true
  };

let getCodeBlocks content => content |> codeBlocks |> Js.Array.map getCode;

let hasRefmt content => contains content "refmt";

let handleReady _ => Js.log "ready!";

let codifyString code lang => "\n```" ^ lang ^ "\n" ^ code ^ "\n" ^ "```";

let handleMessage message => {
  let content = getContent message;
  let respond = hasRefmt content;
  let codeBlocks = getCodeBlocks content;
  let result =
    switch (Js.Array.pop codeBlocks) {
    | Some block => Some (block |> RefmtBS.refmt |> RefmtBS.parse)
    | None => None
    };
  switch (respond, result) {
  | (true, Some (Ok res)) =>
    reply message (codifyString res.outText (syntaxLang res.outLang))
  | _ => ()
  }
};

onReady discordClient handleReady;

onMessage discordClient handleMessage;

login discordClient "";

Js.log "trying this thing";
