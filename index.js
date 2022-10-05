import { runMain } from "./dist/ddf-validation-ng.js"

function validate(fp) {
    runMain(fp)();
}

export { validate }