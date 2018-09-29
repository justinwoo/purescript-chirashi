function VariantError(variant) {
  this.variant = variant;
  this.message = "VariantError";
  return this;
}

VariantError.prototype = Object.create(Error.prototype);
VariantError.prototype.constructor = VariantError;
VariantError.prototype.name = "VariantError";

exports._mkVariantError = function(variant) {
  return new VariantError(variant);
};

exports._readVariantError = function(test, nothing, just, error) {
  if (error instanceof VariantError) {
    var variant = error.variant;
    if (test(variant.type)) {
      return just(error);
    } else {
      return nothing;
    }
  } else {
    return nothing;
  }
};

exports._getVariant = function(variantError) {
  return variantError.variant;
};
