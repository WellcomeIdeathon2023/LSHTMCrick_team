import { parse, stringify } from "https://deno.land/std/encoding/yaml.ts";
import { parse as flag} from "https://deno.land/std/flags/mod.ts";

const flags = flag(Deno.args, {string: [
    "script", "yml", "section", "subsection", "fixed", "generated"
]});

//// Gather the constituent parameters

// From the source yaml
var perPage;
try {
    let ind=parseInt(flags.subsection);
    perPage=parse(Deno.readTextFileSync(flags.yml))[ind-1];
    let defaultParams={section: flags.section, subsection: ind};
    if (! perPage.hasOwnProperty("params")) {
	perPage.params=defaultParams;
    } else {
	perPage.params = {...perPage.params,...defaultParams};
    }
} catch(error) {
    perPage = {title: "(default)",
	       description: "",
	       categories: [],
	       params: {section: flags.section, subsection: 1}};
}

// From the script
const script = Deno.readTextFileSync(flags.script);
const yamlRegEx = /^---\s*$/gm;
var match = yamlRegEx.exec(script);
const start = match.index;
match = yamlRegEx.exec(script);
const end = match.index;
var fromScript=parse(script.substring(start,end).replace(yamlRegEx,""));

// From the project-wide params in the resource folder
const fixed = parse(Deno.readTextFileSync(flags.fixed));

// From the make-generated project-wide file
const generated = parse(Deno.readTextFileSync(flags.generated));

// Put them all together
const params = {...fixed, ...fromScript.params, ...generated, ...perPage.params};
delete fromScript.params;
delete perPage.params;

perPage.title = (fromScript.title || "")  + " " + (perPage.title || "");
perPage.description = (fromScript.description || "") + " " + (perPage.description || "");
perPage.categories = [...new Set((perPage.categories || []).concat((fromScript.categories || [])))];

var meta = {...fromScript, ...perPage};
meta.params=params;

//And write new header followed by the script
console.log("---");
console.log(stringify(meta));
console.log(script.substring(end));
