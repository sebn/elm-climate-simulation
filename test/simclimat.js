var { Elm } = require('./elm');
var assert = require('assert-diff');
var test = require('baretest')('elm-climate');
var _ = require('lodash');
var {
    CExperienceValues,
    CModel,
    CSimulationValues,
} = require("simclimat-lib");

assert.options.strict = true

var elmTestWrapper = Elm.TestWrapper.init();

// FIXME: Force same precision in SimClimat as in Elm
Math.exp = x => Math.pow(Math.E, x);

test('pre-industrial', async () => {
    const sv = new CSimulationValues();
    sv.create_1750_state();
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('pre-industrial, fixed concentration', async () => {
    const sv = new CSimulationValues();
    sv.create_1750_state();
    sv.fixed_concentration = true;
    sv.coo_concentr_value = 345;
    await assertSameResultsAsync(sv);
});

test('actual', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, fixed concentration', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.fixed_concentration = true;
    sv.coo_concentr_value = 345;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, fixed eau', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.fixed_eau = true;
    sv.rapport_H2O_value = 0;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, debranche biologie', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.debranche_biologie = true;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, fixed ocean', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.fixed_ocean = true;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, debranche_ocean', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.debranche_ocean = true;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, fixed_albedo', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.fixed_albedo = true;
    sv.albedo_value = 42;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, puit_bio_value=42', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.puit_bio_value = 42;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, puit_oce_value=42', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.puit_oce_value = 42;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, puissance_soleil_value=42', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.puissance_soleil_value = 42;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, distance_ts_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.distance_ts_value = 1.56789e11;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, obliquite_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.obliquite_value = 23.4;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, excentricite_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.excentricite_value = 0.0166;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, precession_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.precession_value = 102.8;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, alteration_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.alteration_value = 123;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, emit_anthro_coo_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.emit_anthro_coo_value = 8.1;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, volcan_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.volcan_value = 0.085;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('actual, stockage_biologique_value', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    sv.stockage_biologique_value = 0.001;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

var assertSameResultsAsync = async sv => {
    const elmResult = await runElmClimateAsync(sv);

    if (typeof elmResult == 'string') {
        assert.fail(elmResult);
    } else {
        assert.deepEqual(
            runSimClimat(sv),
            elmResult
        );
    }
};

var runElmClimateAsync = sv =>
    new Promise((resolve, reject) => {
        let unsubscribe;
        try {
            unsubscribe = elmTestWrapper.ports.output.subscribe(svOut => {
                resolve(normalizeSimulationValues(svOut));
            });
            elmTestWrapper.ports.input.send({
                ...sv,
                temperature_data: {
                    datas: [],
                    past_datas: [],
                    resolution: 100,
                    indice_min: 0,
                    indice_max: 100,
                    imin: 0,
                    imax: 100
                }
            });
        } catch (err) {
            reject(err);
        } finally {
            if (unsubscribe != null) {
                unsubscribe();
            }
        }
    });

var runSimClimat = sv => {
    const years /*: numbers */ = 10000;
    // const sv /*: CSimulationValues */ = new CSimulationValues();
    const ev /*: CExperienceValues */ = new CExperienceValues(years);
    const m /*: CModel */ = new CModel();

    // Here you should call either create_actual_state() or create_1750_state() of CSimulationValues object to init properties and datas arrays.
    // sv.create_1750_state();

    // Here you can change CSimulationValues properties before calling modelExecute
    // sv.fixed_albedo = true;
    // sv.albedo_value = 33;

    // Call modelExecute method to calculate climates datas
    return normalizeSimulationValues(
        m.modelExecute(sv, ev)
    );
};

var logSimulationValues = sv => {
    // console.log(JSON.stringify(normalizeSimulationValues(sv), null, 2));
    return sv;
}

var normalizeSimulationValues = _.identity;

test.run();
