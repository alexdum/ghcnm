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

$(document).ready(function () {
    // Layer Control Interactions (using delegation for robustness)
    $(document).on('mouseenter', '.map-layer-control', function () {
        $(this).addClass('expanded');
    });

    $(document).on('mouseleave', '.map-layer-control', function () {
        $(this).removeClass('expanded');
    });

    $(document).on('change', '.map-layer-control input[type="radio"]', function () {
        $('.map-layer-control').removeClass('expanded');
    });
});
