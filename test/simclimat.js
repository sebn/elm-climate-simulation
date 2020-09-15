const {
    CExperienceValues,
    CModel,
    CSimulationValues,
} = require("simclimat-lib");

let years /*: numbers */ = 10000;
let sv /*: CSimulationValues */ = new CSimulationValues();
let ev /*: CExperienceValues */ = new CExperienceValues(years);
let m /*: CModel */ = new CModel();

// Here you should call either create_actual_state() or create_1750_state() of CSimulationValues object to init properties and datas arrays.
sv.create_1750_state();

// Here you can change CSimulationValues properties before calling modelExecute
sv.fixed_albedo = true;
sv.albedo_value = 33;

// Call modelExecute method to calculate climates datas
sv = m.modelExecute(sv, ev);

// Inspect returned values in console
console.info(JSON.stringify(sv, null, 2));