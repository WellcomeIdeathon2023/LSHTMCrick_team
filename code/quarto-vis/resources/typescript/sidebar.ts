import { parse, stringify } from "https://deno.land/std/encoding/yaml.ts";
import { parse as flag} from "https://deno.land/std/flags/mod.ts";

const flags = flag(Deno.args, {
    string: ["template", "staging", "tag", "repo","sections"]
});

var section;
var subsection;
const sectionNames=flags.sections.split(",");
const quarto = parse(Deno.readTextFileSync(flags.template));
const qmdre = new RegExp(flags.tag.concat(".qmd$"));
const qmds = Array.from(Deno.readDirSync(flags.staging))
    .filter( f => qmdre.test(f.name))
    .map(f => {
	[section, subsection] = f.name.replace(qmdre,"").split("_");
	return {qmd: f.name, isection: sectionNames.indexOf(section), subsection: parseInt(subsection)};
    })
    .sort((a,b) => {
	var cmp = a.isection-b.isection;
	if (cmp==0) {
	    return a.subsection-b.subsection;
	} else {
	    return cmp;
	}
    });


const sections = sectionNames.map(s => ({section: s, contents: []}));

for (const q in qmds) {
    sections[qmds[q].isection].contents.push(qmds[q].qmd);
}

quarto.website.sidebar.contents = quarto.website.sidebar.contents.concat(sections);

const gh=quarto.website.navbar.right.findIndex(s => s.text=="Github repository");
if (gh != -1) {
    quarto.website.navbar.right[gh].href = flags.repo;
}
quarto.project.render = ["index.qmd"].concat(qmds.map(q => q.qmd));
console.log(stringify(quarto));
