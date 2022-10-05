import { runMain } from "./output/Main/index.js"

function validate(fp) {
    runMain(fp)();
}

export { validate }