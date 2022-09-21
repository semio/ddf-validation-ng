import { parse } from 'csv-parse/sync';


// fs.createReadStream(path.resolve(__dirname, 'assets', 'parse.csv'))
//     .pipe(csv.parse({ headers: true }))
//     .on('error', error => console.error(error))
//     .on('data', row => console.log(row))
//     .on('end', rowCount => console.log(`Parsed ${rowCount} rows`));


export function readCsvImpl(csvLine) {
    return parse(csvLine, {
        columns: false,
        relax_column_count: true,
        on_record: (record, { error }) => {
            if (error) {
                console.log(error.message);  // MAYBE: move the checking to purescript code and log it with other errors
                return null
            } else {
                return record
            }
        }
    });
}

// console.log(testCSV("a,b,c\n1,2,3"))