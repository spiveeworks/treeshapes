function [dets, vals, rvecs, lvecs] = program()
    mats = LoadMats4();
    [n, ~, nummats] = size(mats);

    dets = zeros(1, nummats);
    vals = zeros(n, n, nummats);
    lvecs = zeros(n, n, nummats);
    rvecs = zeros(n, n, nummats);
    for i = 1:nummats
        mat = normalize(mats(:,:,i));
        dets(i) = abs(det(mat));
        [V,D,W] = eig(mat);
        vals(:,:,i) = D;
        lvecs(:,:,i) = W;
        rvecs(:,:,i) = V;
    end
end

function out = normalize(mat)
    [n, ~] = size(mat);
    out = mat;
    for i = 1:n
        vec = mat(:,i);
        out(:,i) = vec ./ norm(vec);
    end
end
