// GHCNM Explorer JavaScript Bridge

$(document).on('shiny:connected', function () {
    // console.log("Shiny connected to bridge");
});

// Update parent page URL and context
Shiny.addCustomMessageHandler('updateParentURL', function (message) {
    // console.log("Updating parent URL with GHCNM state:", message);

    // Send message to parent window (Quarto page)
    if (window.parent !== window) {
        window.parent.postMessage({
            type: 'ghcnm-state-update',
            station: message.station || null,
            stationName: message.stationName || null,
            country: message.country || null,
            parameter: message.parameter || null,
            month: message.month || null,
            yearStart: message.yearStart || null,
            yearEnd: message.yearEnd || null
        }, '*');
    }
});
